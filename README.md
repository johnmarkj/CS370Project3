# CS370-JJONESPROJECT3

This is a program I wrote for my COBOL class (CS370). It takes an unsorted order report from a ficticious company
and creates an ordered product analysis. This program sorts the text file input, 'PR3FA17.txt', and then computes
the total value of products sold by the company over the last 3 months of the year. Total values are printed by
causing control breaks on 3 fields which are the customer, State, and City. The incoming data from the file is then
validated and printed in a structured order report. Lastly after all of the detail lines have been printed, a grand
total line is displayed showing the total value of all sales from the last 3 months of the year for the entire company.

## Installations

**ONCE THE CYGWIN FOLDER IS DOWNLOADED MOVE JJONESPROJECT3 AND PR3FA17.TXT FILE TO THE NEW CYGWIN FOLDER**

**For Windows**
[Cygwin Zip File](https://docs.google.com/open?id=0B4CSrXwYHmn_X1dJLS1XcWFfWGs)
- Go to the link and download the zip file
- Extract the zip file(recommended to extract on the desktop)
- Execute the Cygwin Terminal


**For MAC**
- Go to App store 
- Get Xcode and install it
- Go to macports.org
- Select Installing MacPorts from the menu
- Then Download the version for your version of OS X 
- To open and install go to the downloads folder in finder and right click and select OPEN.  it does matter this circumvents 
gatekeeper and allows you to install whatever you want.
 
- open terminal
- Type xcode-select —install
- Type sudo port selfupdate
- You will be prompted for your password after you enter your password then type sudo port install open-cobol You are good to go.


## Deployment
- Make sure all of the files are in the cygwin-open-cobol folder
- Run Cygwin Terminal.vbs
- Change the directory to the location that has the project file and input file
- Type this in the command line  **cobc -xo Project3.exe --std=mf JJONESPROJECT3.cbl**
- This will create an executable file, Run the exe **./Project3.exe**

### **Version 1.0**

#### Author
*John M. Jones*

##### **Notes**
This project was made to show how multi-level control breaks function as well as how to use tables and arrays.
It has sorting and validation of data as well, possible additions could be merging files together or tweaking the file to add more states or cities.

