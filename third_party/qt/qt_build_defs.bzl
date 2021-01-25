# This is in its own file so that Qt build logic can be modified without
# modifying the `qt.BUILD` file itself, which would cause a re-fetch and rebuild
# of Qt, which sucks.

QtInfo = provider(fields = ["runtime_files", "root"])

def define_qt_targets():
    qt_components = [
        "Bodymovin",
        "Core",
        "DBus",
        "Gui",
        "Network",
        "OpenGL",
        "Pdf",
        "PrintSupport",
        "Qml",
        "QmlModels",
        "QmlWorkerScript",
        "Quick",
        "QuickShapes",
        "QuickWidgets",
        "Sql",
        "WebChannel",
        "WebEngine",
        "WebEngineCore",
        "WebEngineWidgets",
        "WebSockets",
        "Widgets",
    ]

    qt_modules = ["Qt5{}".format(x) for x in qt_components]
    win_qt_modules = [
        "libEGL",
        "libGLESv2",
    ] + qt_modules
    linux_qt_modules = ["Qt5XcbQpa"] + qt_modules

    win_lib_filepaths = (
        ["lib/{}.lib".format(x) for x in win_qt_modules]
    )
    linux_lib_filepaths = (
        ["lib/lib{}.so.5".format(x) for x in linux_qt_modules]
    )

    include_directories = ["include"] + ["include/Qt{}".format(x) for x in qt_components]

    win_shared_library_filepaths = ["bin/{}.dll".format(x) for x in win_qt_modules]
    linux_shared_library_filepaths = ["lib/lib{}.so".format(x) for x in linux_qt_modules]

    resources = [
        "resources/icudtl.dat",
        "resources/qtwebengine_devtools_resources.pak",
        "resources/qtwebengine_resources.pak",
        "resources/qtwebengine_resources_100p.pak",
        "resources/qtwebengine_resources_200p.pak",
    ]

    win_platforms_plugins = [
        "plugins/platforms/qwindows.dll",
    ]
    linux_platforms_plugins = [
        #"plugins/platforms/libqeglfs.so",
        "plugins/platforms/libqxcb.so",
        #"plugins/platforms/libqlinuxfb.so",
        #"plugins/platforms/libqminimalegl.so",
        #"plugins/platforms/libqminimal.so",
        #"plugins/platforms/libqoffscreen.so",
        #"plugins/platforms/libqvnc.so",
        #"plugins/platforms/libqwebgl.so",
    ]

    translations = [
        "translations/qtwebengine_locales/en-US.pak",
    ]

    # Files which are expected to live as sibling files to the final executable.
    native.filegroup(
        name = "qt_data_files",
        srcs = resources + translations,
        visibility = ["//visibility:public"],
    )

    native.filegroup(
        name = "qt_platforms_plugins",
        srcs = select({
            "@bazel_tools//src/conditions:windows": win_platforms_plugins,
            "//conditions:default": linux_platforms_plugins,
        }),
        visibility = ["//visibility:public"],
    )

    native.filegroup(
        name = "qt_data_sibling_files",
        srcs = select({
            "@bazel_tools//src/conditions:windows": ["bin/QtWebEngineProcess.exe"],
            "//conditions:default": [],
        }),
        visibility = ["//visibility:public"],
    )

    native.cc_library(
        name = "qt_lib",
        srcs = select({
            "@bazel_tools//src/conditions:windows": win_lib_filepaths + win_shared_library_filepaths,
            "//conditions:default": linux_lib_filepaths,
        }),
        hdrs = native.glob([x + "/**" for x in include_directories]),
        includes = include_directories,
        visibility = ["//visibility:public"],
        deps = [
            "@vulkan_sdk//:vulkan",
        ],
    )

    native.alias(
        name = "moc",
        actual = select({
            "@bazel_tools//src/conditions:windows": "bin/moc.exe",
            "//conditions:default": "bin/moc",
        }),
        visibility = ["//visibility:public"],
    )
    native.alias(
        name = "uic",
        actual = select({
            "@bazel_tools//src/conditions:windows": "bin/uic.exe",
            "//conditions:default": "bin/uic",
        }),
        visibility = ["//visibility:public"],
    )
    native.alias(
        name = "rcc",
        actual = select({
            "@bazel_tools//src/conditions:windows": "bin/rcc.exe",
            "//conditions:default": "bin/rcc",
        }),
        visibility = ["//visibility:public"],
    )
