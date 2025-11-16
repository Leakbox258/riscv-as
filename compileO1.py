#! /usr/bin/env python3

import os
import subprocess
from sys import argv

SOURCE_FILE_PATH = "./3rd-party/gnalc/test/contest"
GNALC_PATH = argv[1]
OUTUT_DIR = "./test"

if os.path.exists(OUTUT_DIR) is False:
  os.mkdir(OUTUT_DIR)

testcases_cnt  = 0

for root, dirs, files in os.walk(SOURCE_FILE_PATH):
  
  for file in files:
    
    if file[0] == '.' or file == "README.md":
      continue
    if file.find(".in") != -1 or file.find(".out") != -1:
      continue
    if file.find("86") != -1:
      # gnalc stackoverflow
      continue
    
    source_file = os.path.join(root, file)
    rel_dir = os.path.relpath(root, SOURCE_FILE_PATH)
    out_dir = os.path.join(OUTUT_DIR, rel_dir)
    os.makedirs(out_dir, exist_ok=True)
    asm_file = os.path.join(out_dir, os.path.splitext(file)[0] + ".s")
    subprocess.run([GNALC_PATH, "-S", source_file, "-march=riscv64", "-O1", "-o", asm_file], check=True)
    print(f"{testcases_cnt}: {source_file} -> {asm_file}")
    testcases_cnt += 1
	