
"""
This script scans a directory for all .xml files. It creates new versions of those files, with the same name but with _ibm or _rrw appended to the end of the name.
The new files will be identical to the original ones, except that the run.id will be set to "ibm" or "rrw" respectively. The purpose of this is to allow for easy running of the same analyses with different models.
"""

import os
import sys
import shutil

def scan_and_extract(root_dir):
    if not os.path.exists(root_dir):
        print(f"{root_dir} does not exist")
        return

    list_of_config_files = []

    for dirpath, dirnames, filenames in os.walk(root_dir):
        for filename in filenames:            
            if filename.endswith('.xml') and filename.find('ibm') == -1 and filename.find('rrw') == -1 and filename.find('tree') == -1 and filename.find('config') > 0:
                file_path = os.path.join(dirpath, filename)
                list_of_config_files.append(file_path)
                print(f"Found {file_path}")   

    return(list_of_config_files)

def generate_final_config_files(list_of_config_files,model):
    if(model == "ibm"): 
        for one_config_file in list_of_config_files:    
            shutil.copyfile(one_config_file, one_config_file.replace(".xml","_ibm.xml"))
            print(f"Extracting {one_config_file}")

    elif(model == "rrw"):
        for one_config_file in list_of_config_files:
            shutil.copyfile(one_config_file, one_config_file.replace(".xml","_rrw.xml"))
            print(f"Extracting {one_config_file}")
            with open(one_config_file.replace(".xml","_rrw.xml"), 'r') as f:
                data = f.read()
            data = data.replace('run.id=\"ibm\"', 'run.id=\"rrw\"')
            data = data.replace('name=\"ibm\"', 'name=\"rrw+gamma\"')
            with open(one_config_file.replace(".xml","_rrw.xml"), 'w') as f:
                f.write(data)
    return

print(f"Working directory: {os.getcwd()}")
generic_config_files = scan_and_extract(".")
generate_final_config_files(generic_config_files,"ibm")
generate_final_config_files(generic_config_files,"rrw")
