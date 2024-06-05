
import os
import subprocess

for year in range(2000,2008) :
    
    dir_path = "./" + str(year) + "/";
    
    # list to store files
    res = []
    
    # Iterate directory
    for file_path in os.listdir(dir_path):
    # check if current file_path is a file
        if os.path.isfile(os.path.join(dir_path, file_path)):
            # add filename to list
            res.append(file_path);
            
            
    for file in res:
        if file.find("_phyrex_tree_ibm.txt") > -1 : 
            treefile = file;
                    
                    
    for file in res:
        if file.find(".xml") > -1 : 
            xmlfile = file;
                            
                            
    predict = "./predict -u " + dir_path + treefile + " --xml=" + dir_path + xmlfile + " " + str(year) + " " + str(year - 1) + " 0.2 > " + dir_path + "raw_predict";
                            

    subprocess.run([predict],shell=True);
    subprocess.run(["grep XXX " + dir_path + "raw_predict" + " > " + dir_path + "clean_predict"],shell=True); 
    subprocess.run(["find " + dir_path + "clean_predict -type f -exec sed -i -e 's/. XXX/ /g' {} ;"],shell=True);
    subprocess.run(["find " + dir_path + "wnv_incidence_" + str(year) + ".csv" + " -type f -exec sed -i -e 's/;/,/g' {} ;"],shell=True);

subprocess.run(["R CMD BATCH ./map_predict.R"],shell=True);
