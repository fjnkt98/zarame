const std = @import("std");

pub fn build(b: *std.Build) void {
    // ----------------------------- options -----------------------------
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // ----------------------------- libraries -----------------------------
    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "zarame",
        .root_module = lib_mod,
    });
    b.installArtifact(lib);

    // ----------------------------- executables -----------------------------
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_mod.addImport("zarame", lib_mod);

    const exe = b.addExecutable(.{
        .name = "zarame",
        .root_module = exe_mod,
    });
    b.installArtifact(exe);

    // ----------------------------- commands -----------------------------
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // ----------------------------- dictionary -----------------------------
    const builder = b.createModule(.{
        .root_source_file = b.path("src/builder.zig"),
        .target = target,
        .optimize = optimize,
    });
    builder.addImport("zarame", lib_mod);

    const builder_exe = b.addExecutable(.{
        .name = "builder",
        .root_module = builder,
    });

    const run_builder = b.addRunArtifact(builder_exe);
    run_builder.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_builder.addArgs(args);
    }
    const builder_step = b.step("dictionary", "build dictionary");
    builder_step.dependOn(&run_builder.step);

    // ----------------------------- tests -----------------------------
    const lib_unit_tests = b.addTest(.{
        .root_module = lib_mod,
    });
    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const builder_unit_tests = b.addTest(.{
        .root_module = builder,
    });
    const run_builder_unit_tests = b.addRunArtifact(builder_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
    test_step.dependOn(&run_builder_unit_tests.step);
}
