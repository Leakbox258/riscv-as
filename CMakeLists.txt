cmake_minimum_required(VERSION 3.20)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_STANDARD 26)

project(RV-Assembler)

add_compile_options("-fno-rtti")
add_compile_options("-Wall")
add_compile_options("-Wextra")

if(${CMAKE_BUILD_TYPE} MATCHES Debug)
	add_compile_options("-g" "-fstandalone-debug" "-fno-omit-frame-pointer")
	add_link_options("-rdynamic")
	option(ENABLE_ASAN "Enable AddressSanitizer" OFF)			
elseif(${CMAKE_BUILD_TYPE} MATCHES ASAN)
	option(ENABLE_ASAN "Enable AddressSanitizer" ON)			
endif()

if(ENABLE_ASAN)
	message(STATUS "AddressSantitizer enable")
	add_compile_options("-fsanitize=address" "-fno-omit-frame-pointer")
	add_link_options(-fsanitize=address)
endif()

include_directories(include)

aux_source_directory(lib/mc MC)
aux_source_directory(lib/parser PARSER)
aux_source_directory(lib/utils UTILS)
aux_source_directory(lib/utils/ADT UTILS_ADT)

set(ASSEMBLER ${MC} ${PARSER} ${UTILS} ${UTILS_ADT})

add_executable(assembler lib/main.cpp ${ASSEMBLER})
add_executable(test-extStl lib/STL.cpp ${ASSEMBLER})
add_executable(test-pseudo lib/pseudo.cpp ${ASSEMBLER})