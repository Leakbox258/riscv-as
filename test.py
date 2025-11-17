#! /usr/bin/env python3

import os
from sys import argv
import subprocess
import datetime

ASM_FILE_DIR = "./test"
ASSEMBLER_PATH = "./build/test-assembler"
TEST_OPTION = argv[1]
TEST_RESULT_DIR = f"./test_result/{datetime.datetime.now()}"

# deal test flag
if TEST_OPTION.count("h"):
  print('''  -h/--help print help infos
  -a test assembler only
  -al test assembler and link with sylib
  -alr test assembler, link with sylib and running checks''')
  exit(0)
 
for root, dirs, files in os.walk(ASM_FILE_DIR):
  for file in files:
    source_asm = f'{root}/{file}'
    result_obj = f'{TEST_RESULT_DIR}/{file.replace(".s", ".o")}'
  
    if TEST_OPTION.count('a'):		
      subprocess.run([ASSEMBLER_PATH, "-c", source_asm, "-o", result_obj], check=True, stdout="/dev/null")
   
    if TEST_OPTION.count('l'):
      print("-l not supported yet")
      exit(-1)
   
    if TEST_OPTION.count('r'):
      print("-r not supported yet")
      exit(-2)