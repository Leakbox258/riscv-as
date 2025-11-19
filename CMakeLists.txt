cmake_minimum_required(VERSION 3.20)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)
set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_STANDARD 26)

add_subdirectory(3rd-party)

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

# test with gnalc
set(GNALC_RV64 $<TARGET_FILE:gnalc_rv64>)
set(GNALC_TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR}/test)
set(SYLIB_PATH ${CMAKE_CURRENT_SOURCE_DIR}/3rd-party/gnalc/test/sylib/sylib.c)

# ----- Stamp files placed in binary dir -----
set(DIR_STAMP  ${CMAKE_BINARY_DIR}/gnalc_dir.stamp)
set(ASM_STAMP  ${CMAKE_BINARY_DIR}/gnalc_asm.stamp)

message(STATUS "gnalc_rv64 path = ${GNALC_RV64}")

add_custom_command(
    OUTPUT ${DIR_STAMP}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${GNALC_TEST_DIR}
    COMMAND ${CMAKE_COMMAND} -E touch ${DIR_STAMP}
    COMMENT "Create gnalc test directory"
)

add_custom_command(
    OUTPUT ${ASM_STAMP}
    COMMAND python3 ${CMAKE_CURRENT_SOURCE_DIR}/compileO1.py ${GNALC_RV64}
    COMMAND ${CMAKE_COMMAND} -E touch ${ASM_STAMP}
    DEPENDS ${DIR_STAMP} gnalc_rv64
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generate RV64 Asm Files"
		VERBATIM
)

set(SYLIB_O ${CMAKE_CURRENT_SOURCE_DIR}/test/sylib.o)
set(SYLIB_A ${CMAKE_CURRENT_SOURCE_DIR}/test/sylib.a)

add_custom_command(
	OUTPUT ${SYLIB_O}
	COMMAND sh -c "riscv64-linux-gnu-gcc -c ${SYLIB_PATH} -o ${SYLIB_O}"
	COMMENT "Generate sylib.o RV64"
)

add_custom_command(
	OUTPUT ${SYLIB_A}
	COMMAND sh -c "ar -rcs ${SYLIB_A} ${SYLIB_O}"
	DEPENDS ${SYLIB_O}
	COMMENT "Generate sylib.a RV64"
)

add_custom_target(generate_gnalc_artifacts DEPENDS ${ASM_STAMP} ${SYLIB_A})

add_executable(test-assembler lib/main.cpp ${ASSEMBLER})
add_dependencies(test-assembler generate_gnalc_artifacts)
