cmake_minimum_required(VERSION 3.17)
project(CMakePlayground C)

set(CMAKE_C_STANDARD 11)

add_subdirectory(DFA-Samples/Sample1)
add_subdirectory(DFA-Samples/Sample2)

add_executable(CMakePlayground main.c)
