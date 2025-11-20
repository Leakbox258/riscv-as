#! /usr/bin/env python3

import os
from sys import argv
import subprocess
import datetime

ASM_FILE_DIR = "./test"
TEST_OPTION = argv[1]
TEST_RESULT_DIR = f"./test_result/{datetime.datetime.now()}"

ASSEMBLER_PATH = "./build/test-assembler"
SYLIB_PATH = "./test/sylib.a"
RISCV64_LINUX_GNU_GCC = "riscv64-linux-gnu-gcc"

# deal test flag
if TEST_OPTION.count("h"):
  print('''  -h/--help print help infos
  -a test assembler only
  -al test assembler and link with sylib
  -alr test assembler, link with sylib and running checks''')
  exit(0)
 
 
if os.path.exists("./test_result") is False:
  os.mkdir("./test_result")
  
os.mkdir(TEST_RESULT_DIR)
  
for root, dirs, files in os.walk(ASM_FILE_DIR):
  for file in files:
    if file == "sylib.o" or file == "sylib.a":
      continue
    
    source_asm = f'{root}/{file}'
    result_obj = f'{TEST_RESULT_DIR}/{file.replace(".s", ".o")}'
    result_elf = f'{TEST_RESULT_DIR}/{file.replace(".s", "")}'
  
    if TEST_OPTION.count('a'):
      try:		
        subprocess.run([ASSEMBLER_PATH, "-c", source_asm, "-o", result_obj], check=True, stdout=True)
      except subprocess.CalledProcessError:
        print(f"[!] Assembler test failed for {source_asm}")
        exit(-1)
      print(f"[+] Assembler test passed for {source_asm}, output: {result_obj}")

    if TEST_OPTION.count('l'):
      try:
        subprocess.run([RISCV64_LINUX_GNU_GCC, result_obj, "-o", result_elf], check=True, stdout=True)
      except subprocess.CalledProcessError:
        print(f"[!] Linking test failed for {source_asm}")
        exit(-1)
      print(f"[+] Linking test passed for {source_asm}")

    if TEST_OPTION.count('r'):
      print("-r not supported yet")
      exit(-2)