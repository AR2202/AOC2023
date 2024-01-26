load("@claro-lang//:rules.bzl", "claro_binary", "claro_module")

claro_binary(
    name="aoc2023_bin",
    main_file="aoc2023.claro",
    resources={
        "Input": "input.txt",
    },
    deps={
        "Style": ":styling",
    },
)
claro_binary(
    name="day1",
    main_file="day1.claro",
    resources={"Input": "Day01.txt"},
)
claro_binary(
    name="day3",
    main_file="day3.claro",
    resources={"Input": "Day03.txt"},
)

claro_binary(
    name="day13",
    main_file="day13.claro",
     resources={"Input": "day13.txt"},
   
)
claro_binary(
    name="hello_world",
    main_file="hello_world.claro",
    deps={
        "Style": ":styling",
    },
)

claro_binary(
    name="fizzbuzz",
    main_file="fizzbuzz.claro",
)

claro_module(
    name="styling",
    module_api_file="styling.claro_module_api",
    srcs=["styling/styling.claro"],
)
