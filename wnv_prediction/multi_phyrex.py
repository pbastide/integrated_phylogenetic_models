import os
import sys


def scan_and_extract(root_dir):
    if not os.path.exists(root_dir):
        print(f"{root_dir} does not exist")
        return

    list_of_config_files = []

    for dirpath, dirnames, filenames in os.walk(root_dir):
        for filename in filenames:            
            if filename.endswith('.xml') and filename.find("config") != -1:
                file_path = os.path.join(dirpath, filename)
                list_of_config_files.append(file_path)
                print(f"Found {file_path}")   

    return(list_of_config_files)

def run_phyrex(list_of_config_files):

    for config_file in list_of_config_files:
        print(f"Running phyrex on {config_file} in directory {os.path.dirname(config_file)}")
        os.chdir(os.path.dirname(config_file))
        os.system(f"../../phyrex_install/phyrex --xml={os.path.basename(config_file)}")


print(f"Working directory: {os.getcwd()}")
generic_config_files = scan_and_extract(".")
run_phyrex(generic_config_files)