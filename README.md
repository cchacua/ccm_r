# ccm_r
R repository for the "Cali Come Mejor" project.

# What would I find here?
In this repository, you can have access to a set of functions, scripts and routines for extracting and processing information about 
food consumption in Cali. All the data come from the DANE's (Colombian National Administrative Department of Statistics) 
Income and Expenditure 2006- 2007 National Survey.

# Where are the datasets?
As the dataset files are so big, in this repository I have included only the processing R scripts that allow to extract and consult
information from the original DANE's files. Therefore, in order to use this repository, you have to download the original DANE's 24 
datamodules of the Income and Expenditure 2006- 2007 National Survey and put all the original .txt files in a new folder called ``txt`` inside another folder called ``data``, just outside this folder. Otherwise, you can select a new folder path for those files, by changing the route of the following line, that is located in the ``scripts/main.R`` script:

``modules<-list.files(path="../data/txt", full.names=TRUE)``

# Where can I find more information about the methodology that you used?
You can consult the official methodological aspects of this survey in the DANE's website. However, I have included some of this reference documents inside the folder ``reference_documents``.


# How can I replicate your results?
If you want to get the same results that I got, you can run only the lines of the ``scripts/main.R`` script. It loads the functions of
the other scripts and it put all the results in a new folder called ``output``.


# Contact
If you have any question, suggestion, or if you want to contribute to this project, feel free to fork this repo and to comment.
In addition, you can send me a message to my email 
[cchacua@gmail.com](mailto:cchacua@gmail.com "email")


