const std = @import("std");
const Unit = @import("unit.zig").Unit;
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Parser = @import("parser.zig").Parser;
const Node = @import("parser.zig").Node;
const cg = @import("codegen.zig");
const TypeChecker = @import("typecheck.zig").TypeChecker;
const TypeInterner = @import("types.zig").TypeInterner;

pub fn compileLog(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const level_txt = comptime message_level.asText();
    const level_fmt = switch (message_level) {
        .debug => "\x1b[1;35m",
        .info => "\x1b[1;36m",
        .warn => "\x1b[1;36m",
        .err => "\x1b[1;31m",
    };
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";
    const stderr = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    nosuspend {
        writer.print("cp: " ++ level_fmt ++ level_txt ++ prefix2 ++ "\x1b[0;1m" ++ format ++ "\x1b[0m\n", args) catch return;
        bw.flush() catch return;
    }
}

pub const std_options: std.Options = .{
    .logFn = compileLog,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var argv = std.process.args();
    var source_file: ?[]const u8 = null;
    var output_file: ?[]const u8 = null;

    var defines = std.ArrayList([]const u8).init(gpa.allocator());
    var include_paths = std.ArrayList([]const u8).init(gpa.allocator());
    _ = argv.next(); // program name
    var next_arg_mode: enum { source_file, output_file } = .source_file;
    var link = true;
    var expand = false;
    while (argv.next()) |arg| {
        switch (arg[0]) {
            '-' => {
                switch (arg[1]) {
                    'D' => {
                        try defines.append(arg[2..]);
                    },
                    'I' => {
                        try include_paths.append(arg[2..]);
                    },
                    'o' => {
                        next_arg_mode = .output_file;
                    },
                    'E' => {
                        expand = true;
                    },
                    'c' => link = false,
                    'O' => {},
                    'v' => {
                        const version =
                            \\CP version 0.0.1
                            \\Target: arm64-apple-darwin23.3.0
                            \\Thread model: posix
                            \\InstalledDir: /Users/oliverclarke/Documents/dev/cp/zig-out/bin
                        ;
                        std.debug.print("{s}\n", .{version});
                       return;
                    },
                    else => std.log.warn("Unused argument {s}", .{arg}),
                }
            },
            else => {
                switch (next_arg_mode) {
                    .source_file => source_file = arg,
                    .output_file => output_file = arg,
                }

                next_arg_mode = .source_file;
            },
        }
    }

    if (source_file == null) {
        std.log.err("no input files", .{});
        return;
    }
    std.log.debug("File: {s}", .{source_file.?});

    const source = try std.fs.cwd().readFileAlloc(gpa.allocator(), source_file.?, std.math.maxInt(usize));
    std.debug.print("{s}\n", .{source});

    const full_path = try std.fs.realpathAlloc(gpa.allocator(), ".");
    const absolute_path = try std.fs.path.resolve(gpa.allocator(), &.{
        full_path,
        source_file.?,
    });
    defer gpa.allocator().free(absolute_path);
    std.log.info("CWD: {s}", .{absolute_path});

    var unit = Unit.init(gpa.allocator());
    var interner = TypeInterner.init(&unit);
    interner.setup();
    unit.interner = &interner;

    const config_defines = [_]struct { []const u8, []const u8 }{
        .{ "__STDC__", "" },
        .{ "__arm64__", "" },
        .{ "__INT64_TYPE__", "long long int" },

        .{ "TARGET_IPHONE_SIMULATOR", "0" },
        .{ "TARGET_OS_DRIVERKIT", "0" },
        .{ "TARGET_OS_EMBEDDED", "0" },
        .{ "TARGET_OS_IOS", "0" },
        .{ "TARGET_OS_IPHONE", "0" },
        .{ "TARGET_OS_LINUX", "0" },
        .{ "TARGET_OS_MAC", "1" },
        .{ "TARGET_OS_MACCATALYST", "0" },
        .{ "TARGET_OS_NANO", "0" },
        .{ "TARGET_OS_OSX", "1" },
        .{ "TARGET_OS_SIMULATOR", "0" },
        .{ "TARGET_OS_TV", "0" },
        .{ "TARGET_OS_UIKITFORMAC", "0" },
        .{ "TARGET_OS_UNIX", "0" },
        .{ "TARGET_OS_WATCH", "0" },
        .{ "TARGET_OS_WIN32", "0" },
        .{ "TARGET_OS_WINDOWS", "0" },
        .{ "_LP64", "1" },
        .{ "__AARCH64EL__", "1" },
        .{ "__AARCH64_CMODEL_SMALL__", "1" },
        .{ "__AARCH64_SIMD__", "1" },
        .{ "__APPLE_CC__", "6000" },
        .{ "__APPLE__", "1" },
        .{ "__ARM64_ARCH_8__", "1" },
        .{ "__ARM_64BIT_STATE", "1" },
        .{ "__ARM_ACLE", "200" },
        .{ "__ARM_ALIGN_MAX_STACK_PWR", "4" },
        .{ "__ARM_ARCH", "8" },
        .{ "__ARM_ARCH_ISA_A64", "1" },
        .{ "__ARM_ARCH_PROFILE", "'A'" },
        .{ "__ARM_FEATURE_AES", "1" },
        .{ "__ARM_FEATURE_ATOMICS", "1" },
        .{ "__ARM_FEATURE_BTI", "1" },
        .{ "__ARM_FEATURE_CLZ", "1" },
        .{ "__ARM_FEATURE_COMPLEX", "1" },
        .{ "__ARM_FEATURE_CRC32", "1" },
        .{ "__ARM_FEATURE_CRYPTO", "1" },
        .{ "__ARM_FEATURE_DIRECTED_ROUNDING", "1" },
        .{ "__ARM_FEATURE_DIV", "1" },
        .{ "__ARM_FEATURE_DOTPROD", "1" },
        .{ "__ARM_FEATURE_FMA", "1" },
        .{ "__ARM_FEATURE_FP16_FML", "1" },
        .{ "__ARM_FEATURE_FP16_SCALAR_ARITHMETIC", "1" },
        .{ "__ARM_FEATURE_FP16_VECTOR_ARITHMETIC", "1" },
        .{ "__ARM_FEATURE_FRINT", "1" },
        .{ "__ARM_FEATURE_IDIV", "1" },
        .{ "__ARM_FEATURE_JCVT", "1" },
        .{ "__ARM_FEATURE_LDREX", "0xF" },
        .{ "__ARM_FEATURE_NUMERIC_MAXMIN", "1" },
        .{ "__ARM_FEATURE_PAUTH", "1" },
        .{ "__ARM_FEATURE_QRDMX", "1" },
        .{ "__ARM_FEATURE_RCPC", "1" },
        .{ "__ARM_FEATURE_SHA2", "1" },
        .{ "__ARM_FEATURE_SHA3", "1" },
        .{ "__ARM_FEATURE_SHA512", "1" },
        .{ "__ARM_FEATURE_UNALIGNED", "1" },
        .{ "__ARM_FP", "0xE" },
        .{ "__ARM_FP16_ARGS", "1" },
        .{ "__ARM_FP16_FORMAT_IEEE", "1" },
        .{ "__ARM_NEON", "1" },
        .{ "__ARM_NEON_FP", "0xE" },
        .{ "__ARM_NEON__", "1" },
        .{ "__ARM_PCS_AAPCS64", "1" },
        .{ "__ARM_SIZEOF_MINIMAL_ENUM", "4" },
        .{ "__ARM_SIZEOF_WCHAR_T", "4" },
        .{ "__ARM_STATE_ZA", "1" },
        .{ "__ARM_STATE_ZT0", "1" },
        .{ "__ATOMIC_ACQUIRE", "2" },
        .{ "__ATOMIC_ACQ_REL", "4" },
        .{ "__ATOMIC_CONSUME", "1" },
        .{ "__ATOMIC_RELAXED", "0" },
        .{ "__ATOMIC_RELEASE", "3" },
        .{ "__ATOMIC_SEQ_CST", "5" },
        .{ "__BIGGEST_ALIGNMENT__", "8" },
        .{ "__BITINT_MAXWIDTH__", "128" },
        // .{ "__BLOCKS__", "1" },
        .{ "__BOOL_WIDTH__", "8" },
        .{ "__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__" },
        .{ "__CHAR16_TYPE__", "unsigned short" },
        .{ "__CHAR32_TYPE__", "unsigned int" },
        .{ "__CHAR_BIT__", "8" },
        .{ "__CLANG_ATOMIC_BOOL_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_CHAR16_T_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_CHAR32_T_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_CHAR_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_INT_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_LLONG_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_LONG_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_POINTER_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_SHORT_LOCK_FREE", "2" },
        .{ "__CLANG_ATOMIC_WCHAR_T_LOCK_FREE", "2" },
        .{ "__CONSTANT_CFSTRINGS__", "1" },
        .{ "__DBL_DECIMAL_DIG__", "17" },
        .{ "__DBL_DENORM_MIN__", "4.9406564584124654e-324" },
        .{ "__DBL_DIG__", "15" },
        .{ "__DBL_EPSILON__", "2.2204460492503131e-16" },
        .{ "__DBL_HAS_DENORM__", "1" },
        .{ "__DBL_HAS_INFINITY__", "1" },
        .{ "__DBL_HAS_QUIET_NAN__", "1" },
        .{ "__DBL_MANT_DIG__", "53" },
        .{ "__DBL_MAX_10_EXP__", "308" },
        .{ "__DBL_MAX_EXP__", "1024" },
        .{ "__DBL_MAX__", "1.7976931348623157e+308" },
        .{ "__DBL_MIN_10_EXP__", "(-307)" },
        .{ "__DBL_MIN_EXP__", "(-1021)" },
        .{ "__DBL_MIN__", "2.2250738585072014e-308" },
        .{ "__DECIMAL_DIG__", "__LDBL_DECIMAL_DIG__" },
        .{ "__DYNAMIC__", "1" },
        .{ "__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__", "140000" },
        .{ "__ENVIRONMENT_OS_VERSION_MIN_REQUIRED__", "140000" },
        .{ "__FINITE_MATH_ONLY__", "0" },
        .{ "__FLT16_DECIMAL_DIG__", "5" },
        .{ "__FLT16_DENORM_MIN__", "5.9604644775390625e-8F16" },
        .{ "__FLT16_DIG__", "3" },
        .{ "__FLT16_EPSILON__", "9.765625e-4F16" },
        .{ "__FLT16_HAS_DENORM__", "1" },
        .{ "__FLT16_HAS_INFINITY__", "1" },
        .{ "__FLT16_HAS_QUIET_NAN__", "1" },
        .{ "__FLT16_MANT_DIG__", "11" },
        .{ "__FLT16_MAX_10_EXP__", "4" },
        .{ "__FLT16_MAX_EXP__", "16" },
        .{ "__FLT16_MAX__", "6.5504e+4F16" },
        .{ "__FLT16_MIN_10_EXP__", "(-4)" },
        .{ "__FLT16_MIN_EXP__", "(-13)" },
        .{ "__FLT16_MIN__", "6.103515625e-5F16" },
        .{ "__FLT_DECIMAL_DIG__", "9" },
        .{ "__FLT_DENORM_MIN__", "1.40129846e-45F" },
        .{ "__FLT_DIG__", "6" },
        .{ "__FLT_EPSILON__", "1.19209290e-7F" },
        .{ "__FLT_HAS_DENORM__", "1" },
        .{ "__FLT_HAS_INFINITY__", "1" },
        .{ "__FLT_HAS_QUIET_NAN__", "1" },
        .{ "__FLT_MANT_DIG__", "24" },
        .{ "__FLT_MAX_10_EXP__", "38" },
        .{ "__FLT_MAX_EXP__", "128" },
        .{ "__FLT_MAX__", "3.40282347e+38F" },
        .{ "__FLT_MIN_10_EXP__", "(-37)" },
        .{ "__FLT_MIN_EXP__", "(-125)" },
        .{ "__FLT_MIN__", "1.17549435e-38F" },
        .{ "__FLT_RADIX__", "2" },
        .{ "__FPCLASS_NEGINF", "0x0004" },
        .{ "__FPCLASS_NEGNORMAL", "0x0008" },
        .{ "__FPCLASS_NEGSUBNORMAL", "0x0010" },
        .{ "__FPCLASS_NEGZERO", "0x0020" },
        .{ "__FPCLASS_POSINF", "0x0200" },
        .{ "__FPCLASS_POSNORMAL", "0x0100" },
        .{ "__FPCLASS_POSSUBNORMAL", "0x0080" },
        .{ "__FPCLASS_POSZERO", "0x0040" },
        .{ "__FPCLASS_QNAN", "0x0002" },
        .{ "__FPCLASS_SNAN", "0x0001" },
        .{ "__FP_FAST_FMA", "1" },
        .{ "__FP_FAST_FMAF", "1" },
        // .{ "__GCC_ASM_FLAG_OUTPUTS__", "1" },
        // .{ "__GCC_ATOMIC_BOOL_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_CHAR16_T_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_CHAR32_T_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_CHAR_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_INT_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_LLONG_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_LONG_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_POINTER_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_SHORT_LOCK_FREE", "2" },
        // .{ "__GCC_ATOMIC_TEST_AND_SET_TRUEVAL", "1" },
        // .{ "__GCC_ATOMIC_WCHAR_T_LOCK_FREE", "2" },
        // .{ "__GCC_HAVE_DWARF2_CFI_ASM", "1" },
        // .{ "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1", "1" },
        // .{ "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_16", "1" },
        // .{ "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2", "1" },
        // .{ "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4", "1" },
        // .{ "__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8", "1" },
        // .{ "__GNUC_MINOR__", "2" },
        // .{ "__GNUC_PATCHLEVEL__", "1" },
        // .{ "__GNUC_STDC_INLINE__", "1" },
        // .{ "__GNUC__", "4" },
        // .{ "__GXX_ABI_VERSION", "1002" },
        .{ "__HAVE_FUNCTION_MULTI_VERSIONING", "1" },
        .{ "__INT16_C_SUFFIX__", "" },
        .{ "__INT16_FMTd__", "hd" },
        .{ "__INT16_FMTi__", "hi" },
        .{ "__INT16_MAX__", "32767" },
        .{ "__INT16_TYPE__", "short" },
        .{ "__INT32_C_SUFFIX__", "" },
        .{ "__INT32_FMTd__", "d" },
        .{ "__INT32_FMTi__", "i" },
        .{ "__INT32_MAX__", "2147483647" },
        .{ "__INT32_TYPE__", "int" },
        .{ "__INT64_C_SUFFIX__", "LL" },
        .{ "__INT64_FMTd__", "lld" },
        .{ "__INT64_FMTi__", "lli" },
        .{ "__INT64_MAX__", "9223372036854775807LL" },
        .{ "__INT64_TYPE__", "long long int" },
        .{ "__INT8_C_SUFFIX__", "" },
        .{ "__INT8_FMTd__", "hhd" },
        .{ "__INT8_FMTi__", "hhi" },
        .{ "__INT8_MAX__", "127" },
        .{ "__INT8_TYPE__", "signed char" },
        .{ "__INTMAX_C_SUFFIX__", "L" },
        .{ "__INTMAX_FMTd__", "ld" },
        .{ "__INTMAX_FMTi__", "li" },
        .{ "__INTMAX_MAX__", "9223372036854775807L" },
        .{ "__INTMAX_TYPE__", "long int" },
        .{ "__INTMAX_WIDTH__", "64" },
        .{ "__INTPTR_FMTd__", "ld" },
        .{ "__INTPTR_FMTi__", "li" },
        .{ "__INTPTR_MAX__", "9223372036854775807L" },
        .{ "__INTPTR_TYPE__", "long int" },
        .{ "__INTPTR_WIDTH__", "64" },
        .{ "__INT_FAST16_FMTd__", "hd" },
        .{ "__INT_FAST16_FMTi__", "hi" },
        .{ "__INT_FAST16_MAX__", "32767" },
        .{ "__INT_FAST16_TYPE__", "short" },
        .{ "__INT_FAST16_WIDTH__", "16" },
        .{ "__INT_FAST32_FMTd__", "d" },
        .{ "__INT_FAST32_FMTi__", "i" },
        .{ "__INT_FAST32_MAX__", "2147483647" },
        .{ "__INT_FAST32_TYPE__", "int" },
        .{ "__INT_FAST32_WIDTH__", "32" },
        .{ "__INT_FAST64_FMTd__", "lld" },
        .{ "__INT_FAST64_FMTi__", "lli" },
        .{ "__INT_FAST64_MAX__", "9223372036854775807LL" },
        .{ "__INT_FAST64_TYPE__", "long long int" },
        .{ "__INT_FAST64_WIDTH__", "64" },
        .{ "__INT_FAST8_FMTd__", "hhd" },
        .{ "__INT_FAST8_FMTi__", "hhi" },
        .{ "__INT_FAST8_MAX__", "127" },
        .{ "__INT_FAST8_TYPE__", "signed char" },
        .{ "__INT_FAST8_WIDTH__", "8" },
        .{ "__INT_LEAST16_FMTd__", "hd" },
        .{ "__INT_LEAST16_FMTi__", "hi" },
        .{ "__INT_LEAST16_MAX__", "32767" },
        .{ "__INT_LEAST16_TYPE__", "short" },
        .{ "__INT_LEAST16_WIDTH__", "16" },
        .{ "__INT_LEAST32_FMTd__", "d" },
        .{ "__INT_LEAST32_FMTi__", "i" },
        .{ "__INT_LEAST32_MAX__", "2147483647" },
        .{ "__INT_LEAST32_TYPE__", "int" },
        .{ "__INT_LEAST32_WIDTH__", "32" },
        .{ "__INT_LEAST64_FMTd__", "lld" },
        .{ "__INT_LEAST64_FMTi__", "lli" },
        .{ "__INT_LEAST64_MAX__", "9223372036854775807LL" },
        .{ "__INT_LEAST64_TYPE__", "long long int" },
        .{ "__INT_LEAST64_WIDTH__", "64" },
        .{ "__INT_LEAST8_FMTd__", "hhd" },
        .{ "__INT_LEAST8_FMTi__", "hhi" },
        .{ "__INT_LEAST8_MAX__", "127" },
        .{ "__INT_LEAST8_TYPE__", "signed char" },
        .{ "__INT_LEAST8_WIDTH__", "8" },
        .{ "__INT_MAX__", "2147483647" },
        .{ "__INT_WIDTH__", "32" },
        .{ "__LDBL_DECIMAL_DIG__", "17" },
        .{ "__LDBL_DENORM_MIN__", "4.9406564584124654e-324L" },
        .{ "__LDBL_DIG__", "15" },
        .{ "__LDBL_EPSILON__", "2.2204460492503131e-16L" },
        .{ "__LDBL_HAS_DENORM__", "1" },
        .{ "__LDBL_HAS_INFINITY__", "1" },
        .{ "__LDBL_HAS_QUIET_NAN__", "1" },
        .{ "__LDBL_MANT_DIG__", "53" },
        .{ "__LDBL_MAX_10_EXP__", "308" },
        .{ "__LDBL_MAX_EXP__", "1024" },
        .{ "__LDBL_MAX__", "1.7976931348623157e+308L" },
        .{ "__LDBL_MIN_10_EXP__", "(-307)" },
        .{ "__LDBL_MIN_EXP__", "(-1021)" },
        .{ "__LDBL_MIN__", "2.2250738585072014e-308L" },
        .{ "__LITTLE_ENDIAN__", "1" },
        .{ "__LLONG_WIDTH__", "64" },
        .{ "__LONG_LONG_MAX__", "9223372036854775807LL" },
        .{ "__LONG_MAX__", "9223372036854775807L" },
        .{ "__LONG_WIDTH__", "64" },
        .{ "__LP64__", "1" },
        .{ "__MACH__", "1" },
        .{ "__MEMORY_SCOPE_DEVICE", "1" },
        .{ "__MEMORY_SCOPE_SINGLE", "4" },
        .{ "__MEMORY_SCOPE_SYSTEM", "0" },
        .{ "__MEMORY_SCOPE_WRKGRP", "2" },
        .{ "__MEMORY_SCOPE_WVFRNT", "3" },
        .{ "__NO_INLINE__", "1" },
        .{ "__NO_MATH_ERRNO__", "1" },
        .{ "__OBJC_BOOL_IS_BOOL", "1" },
        .{ "__OPENCL_MEMORY_SCOPE_ALL_SVM_DEVICES", "3" },
        .{ "__OPENCL_MEMORY_SCOPE_DEVICE", "2" },
        .{ "__OPENCL_MEMORY_SCOPE_SUB_GROUP", "4" },
        .{ "__OPENCL_MEMORY_SCOPE_WORK_GROUP", "1" },
        .{ "__OPENCL_MEMORY_SCOPE_WORK_ITEM", "0" },
        .{ "__ORDER_BIG_ENDIAN__", "4321" },
        .{ "__ORDER_LITTLE_ENDIAN__", "1234" },
        .{ "__ORDER_PDP_ENDIAN__", "3412" },
        .{ "__PIC__", "2" },
        .{ "__POINTER_WIDTH__", "64" },
        .{ "__PRAGMA_REDEFINE_EXTNAME", "1" },
        .{ "__PTRDIFF_FMTd__", "ld" },
        .{ "__PTRDIFF_FMTi__", "li" },
        .{ "__PTRDIFF_MAX__", "9223372036854775807L" },
        .{ "__PTRDIFF_TYPE__", "long int" },
        .{ "__PTRDIFF_WIDTH__", "64" },
        .{ "__REGISTER_PREFIX__", "" },
        .{ "__SCHAR_MAX__", "127" },
        .{ "__SHRT_MAX__", "32767" },
        .{ "__SHRT_WIDTH__", "16" },
        .{ "__SIG_ATOMIC_MAX__", "2147483647" },
        .{ "__SIG_ATOMIC_WIDTH__", "32" },
        .{ "__SIZEOF_DOUBLE__", "8" },
        .{ "__SIZEOF_FLOAT__", "4" },
        .{ "__SIZEOF_INT128__", "16" },
        .{ "__SIZEOF_INT__", "4" },
        .{ "__SIZEOF_LONG_DOUBLE__", "8" },
        .{ "__SIZEOF_LONG_LONG__", "8" },
        .{ "__SIZEOF_LONG__", "8" },
        .{ "__SIZEOF_POINTER__", "8" },
        .{ "__SIZEOF_PTRDIFF_T__", "8" },
        .{ "__SIZEOF_SHORT__", "2" },
        .{ "__SIZEOF_SIZE_T__", "8" },
        .{ "__SIZEOF_WCHAR_T__", "4" },
        .{ "__SIZEOF_WINT_T__", "4" },
        .{ "__SIZE_FMTX__", "lX" },
        .{ "__SIZE_FMTo__", "lo" },
        .{ "__SIZE_FMTu__", "lu" },
        .{ "__SIZE_FMTx__", "lx" },
        .{ "__SIZE_MAX__", "18446744073709551615UL" },
        .{ "__SIZE_TYPE__", "long unsigned int" },
        .{ "__SIZE_WIDTH__", "64" },
        .{ "__SSP__", "1" },
        // .{ "__STDC_HOSTED__", "1" },
        // .{ "__STDC_NO_THREADS__", "1" },
        // .{ "__STDC_UTF_16__", "1" },
        // .{ "__STDC_UTF_32__", "1" },
        // .{ "__STDC_VERSION__", "201710L" },
        // .{ "__STDC__", "1" },
        .{ "__UINT16_C_SUFFIX__", "" },
        .{ "__UINT16_FMTX__", "hX" },
        .{ "__UINT16_FMTo__", "ho" },
        .{ "__UINT16_FMTu__", "hu" },
        .{ "__UINT16_FMTx__", "hx" },
        .{ "__UINT16_MAX__", "65535" },
        .{ "__UINT16_TYPE__", "unsigned short" },
        .{ "__UINT32_C_SUFFIX__", "U" },
        .{ "__UINT32_FMTX__", "X" },
        .{ "__UINT32_FMTo__", "o" },
        .{ "__UINT32_FMTu__", "u" },
        .{ "__UINT32_FMTx__", "x" },
        .{ "__UINT32_MAX__", "4294967295U" },
        .{ "__UINT32_TYPE__", "unsigned int" },
        .{ "__UINT64_C_SUFFIX__", "ULL" },
        .{ "__UINT64_FMTX__", "llX" },
        .{ "__UINT64_FMTo__", "llo" },
        .{ "__UINT64_FMTu__", "llu" },
        .{ "__UINT64_FMTx__", "llx" },
        .{ "__UINT64_MAX__", "18446744073709551615ULL" },
        .{ "__UINT64_TYPE__", "long long unsigned int" },
        .{ "__UINT8_C_SUFFIX__", "" },
        .{ "__UINT8_FMTX__", "hhX" },
        .{ "__UINT8_FMTo__", "hho" },
        .{ "__UINT8_FMTu__", "hhu" },
        .{ "__UINT8_FMTx__", "hhx" },
        .{ "__UINT8_MAX__", "255" },
        .{ "__UINT8_TYPE__", "unsigned char" },
        .{ "__UINTMAX_C_SUFFIX__", "UL" },
        .{ "__UINTMAX_FMTX__", "lX" },
        .{ "__UINTMAX_FMTo__", "lo" },
        .{ "__UINTMAX_FMTu__", "lu" },
        .{ "__UINTMAX_FMTx__", "lx" },
        .{ "__UINTMAX_MAX__", "18446744073709551615UL" },
        .{ "__UINTMAX_TYPE__", "long unsigned int" },
        .{ "__UINTMAX_WIDTH__", "64" },
        .{ "__UINTPTR_FMTX__", "lX" },
        .{ "__UINTPTR_FMTo__", "lo" },
        .{ "__UINTPTR_FMTu__", "lu" },
        .{ "__UINTPTR_FMTx__", "lx" },
        .{ "__UINTPTR_MAX__", "18446744073709551615UL" },
        .{ "__UINTPTR_TYPE__", "long unsigned int" },
        .{ "__UINTPTR_WIDTH__", "64" },
        .{ "__UINT_FAST16_FMTX__", "hX" },
        .{ "__UINT_FAST16_FMTo__", "ho" },
        .{ "__UINT_FAST16_FMTu__", "hu" },
        .{ "__UINT_FAST16_FMTx__", "hx" },
        .{ "__UINT_FAST16_MAX__", "65535" },
        .{ "__UINT_FAST16_TYPE__", "unsigned short" },
        .{ "__UINT_FAST32_FMTX__", "X" },
        .{ "__UINT_FAST32_FMTo__", "o" },
        .{ "__UINT_FAST32_FMTu__", "u" },
        .{ "__UINT_FAST32_FMTx__", "x" },
        .{ "__UINT_FAST32_MAX__", "4294967295U" },
        .{ "__UINT_FAST32_TYPE__", "unsigned int" },
        .{ "__UINT_FAST64_FMTX__", "llX" },
        .{ "__UINT_FAST64_FMTo__", "llo" },
        .{ "__UINT_FAST64_FMTu__", "llu" },
        .{ "__UINT_FAST64_FMTx__", "llx" },
        .{ "__UINT_FAST64_MAX__", "18446744073709551615ULL" },
        .{ "__UINT_FAST64_TYPE__", "long long unsigned int" },
        .{ "__UINT_FAST8_FMTX__", "hhX" },
        .{ "__UINT_FAST8_FMTo__", "hho" },
        .{ "__UINT_FAST8_FMTu__", "hhu" },
        .{ "__UINT_FAST8_FMTx__", "hhx" },
        .{ "__UINT_FAST8_MAX__", "255" },
        .{ "__UINT_FAST8_TYPE__", "unsigned char" },
        .{ "__UINT_LEAST16_FMTX__", "hX" },
        .{ "__UINT_LEAST16_FMTo__", "ho" },
        .{ "__UINT_LEAST16_FMTu__", "hu" },
        .{ "__UINT_LEAST16_FMTx__", "hx" },
        .{ "__UINT_LEAST16_MAX__", "65535" },
        .{ "__UINT_LEAST16_TYPE__", "unsigned short" },
        .{ "__UINT_LEAST32_FMTX__", "X" },
        .{ "__UINT_LEAST32_FMTo__", "o" },
        .{ "__UINT_LEAST32_FMTu__", "u" },
        .{ "__UINT_LEAST32_FMTx__", "x" },
        .{ "__UINT_LEAST32_MAX__", "4294967295U" },
        .{ "__UINT_LEAST32_TYPE__", "unsigned int" },
        .{ "__UINT_LEAST64_FMTX__", "llX" },
        .{ "__UINT_LEAST64_FMTo__", "llo" },
        .{ "__UINT_LEAST64_FMTu__", "llu" },
        .{ "__UINT_LEAST64_FMTx__", "llx" },
        .{ "__UINT_LEAST64_MAX__", "18446744073709551615ULL" },
        .{ "__UINT_LEAST64_TYPE__", "long long unsigned int" },
        .{ "__UINT_LEAST8_FMTX__", "hhX" },
        .{ "__UINT_LEAST8_FMTo__", "hho" },
        .{ "__UINT_LEAST8_FMTu__", "hhu" },
        .{ "__UINT_LEAST8_FMTx__", "hhx" },
        .{ "__UINT_LEAST8_MAX__", "255" },
        .{ "__UINT_LEAST8_TYPE__", "unsigned char" },
        .{ "__USER_LABEL_PREFIX__", "_" },
        .{ "__VERSION__", "Homebrew Clang 18.1.7" },
        .{ "__WCHAR_MAX__", "2147483647" },
        .{ "__WCHAR_TYPE__", "int" },
        .{ "__WCHAR_WIDTH__", "32" },
        .{ "__WINT_MAX__", "2147483647" },
        .{ "__WINT_TYPE__", "int" },
        .{ "__WINT_WIDTH__", "32" },
        .{ "__aarch64__", "1" },
        .{ "__arm64", "1" },
        .{ "__arm64__", "1" },
        // .{ "__block", "__attribute__((__blocks__(byref)))" },
        .{ "__clang__", "1" },
        .{ "__clang_literal_encoding__", "UTF-8" },
        .{ "__clang_major__", "18" },
        .{ "__clang_minor__", "1" },
        .{ "__clang_patchlevel__", "7" },
        .{ "__clang_version__", "18.1.7 " },
        .{ "__clang_wide_literal_encoding__", "UTF-32" },
        .{ "__llvm__", "1" },
        .{ "__nonnull", "_Nonnull" },
        .{ "__null_unspecified", "_Null_unspecified" },
        .{ "__nullable", "_Nullable" },
        .{ "__pic__", "2" },
        .{ "__strong", "" },
        .{ "__unsafe_unretained", "" },
        .{ "__weak", "__attribute__((objc_gc(weak)))" },
    };
    try unit.type_names.put("__uint128_t", {});
    try unit.type_names.put("__int128_t", {});
    try unit.type_names.put("__builtin_va_list", {});
    var cfg_source_buffer = std.ArrayList(u8).init(gpa.allocator());
    var cfg_source_writer = cfg_source_buffer.writer();
    for (config_defines) |cfg| {
        try cfg_source_writer.print("#define {s} {s}\n", .{ cfg[0], cfg[1] });
    }
    const cfg_file_index = unit.addFile(absolute_path, cfg_source_buffer.items);

    const root_file = unit.addFile(absolute_path, source);
    // for (defines.items) |def| {
    //     unit.define(def);
    // }
    for (include_paths.items) |path| {
        if (std.fs.path.isAbsolute(path)) {
            try unit.include_dirs.append(path);
        } else {
            try unit.include_dirs.append(try std.fs.realpathAlloc(gpa.allocator(), path));
        }
    }
    var tokenizer = Tokenizer.init(gpa.allocator(), &unit);
    _ = tokenizer.initFile(root_file);
    _ = tokenizer.initFile(cfg_file_index);

    if (expand) {
        var preprocessed_buffer = std.ArrayList(u8).init(gpa.allocator());
        var preprocessed_writer = preprocessed_buffer.writer();

        while (tokenizer.next(false)) |tidx| {
            try unit.writeToken(&preprocessed_writer, tidx);
        }
        var out_file = try std.fs.cwd().createFile("out.cp", .{});
        try out_file.writeAll(preprocessed_buffer.items);

        for (unit.files.items) |file| {
            if (file.tokens.items.len != 1) {
                std.log.info("File {s} {}", .{ file.file_path, file.tokens.items.len });
            }
        }
        return;
    }

    var parser = Parser.init(&unit, &tokenizer);
    const unit_range = try parser.parseUnit();
    std.log.info("Range: {}-{}", .{ unit_range.start, unit_range.count });

    const stdout = std.io.getStdOut();
    var writer = stdout.writer();
    try writer.print("Unit\n", .{});
    for (0..unit_range.count) |i| {
        const node_index = unit.node_ranges.items[i + unit_range.start];
        try Node.writeTree(node_index, &unit, 0, i == unit_range.count - 1, false, writer);
    }

    var typechecker = TypeChecker.init(&unit);
    for (0..unit_range.count) |i| {
        const node_index = unit.node_ranges.items[i + unit_range.start];
        _ = try typechecker.checkNode(node_index, null);
        try Node.writeTree(node_index, &unit, 0, i == unit_range.count - 1, true, writer);
    }

    var codegen = try cg.CodeGenerator.init(&unit);
    for (0..unit_range.count) |i| {
        const node_index = unit.node_ranges.items[i + unit_range.start];
        // try Node.writeTree(node_index, &unit, 0, i == unit_range.count - 1, writer);
        _ = try codegen.genNode(node_index);
    }

    if (link) {
        try codegen.writeToFile("tmp.o");
        var clang_child = std.process.Child.init(&.{ "/opt/homebrew/opt/llvm/bin/clang", "tmp.o", "-o", output_file orelse "a.out" }, gpa.allocator());
        clang_child.stdout_behavior = .Pipe;
        const term = try clang_child.spawnAndWait();
        _ = term;
    } else {
        try codegen.writeToFile(output_file orelse "a.out");
    }

    // builder.write

    // while (tokenizer.next()) |token_index| {
    //     const token = &unit.tokens.items[token_index];
    //     std.log.info("Token({}): {}", .{ token_index, token.* });
    // }
}
