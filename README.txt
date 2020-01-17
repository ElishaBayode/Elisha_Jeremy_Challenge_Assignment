# Elisha_Jeremy_Challenge_Assignment

This is a readme for the R markdown script "Challenge_assignment_report.Rmd".

When correctly used, the script will generate an html file "Challenge_assignment_report.html" containing a report based on the outbreak data provided.

----------------------------------------------------------------------------------------------------------------------------------------------------------------
Basic Workflow:

1. Open "Challenge_assignment_report.Rmd" using RStudio.
2. Ensure necessary libraries are installed (see "library()" commands near the beginning of the script).
3. Click "Knit" and (if prompted) select the folder containing the code and data, and (if prompted) the data file.
The default location for the project is "C:/Users/your_username/Desktop/Smarties/Elisha_Jeremy_Challenge_Assignment", and the default name for the data file is "ebola_2.csv".
----------------------------------------------------------------------------------------------------------------------------------------------------------------

To run this script you should be able to compile R markdown scripts.
In addition, you should have the necessary packages (the argument to the library() commands at the beginning of the script) installed on your system.
Code for installing some of these packages is available (commented-out) at the beginning of the script.

The script by default looks for the directory "C:/Users/your_username/Desktop/Smarties/Elisha_Jeremy_Challenge_Assignment", and if present sets this as the working directory.
If this directory cannot be found, the script will prompt you to choose a directory. Alternatively, you can edit the `wdstring' variable to change the default directory location.

Similarly, the script looks for the data file `ebola_2.csv' in the working directory and will prompt the user to locate the data file if `ebola_2.csv' is not found.
If you would like to run the report with a different dataset, change the `dataName' variable, or run the script without `ebola_2.csv' in the working directory, then
manually select the data file you would like to use.

Expected column names are: "caseID", "householdID", "onsetDate", "deathDate", "reportDate", "status". Additional columns may be added or the order of columns changed,
but the script will not compile if the above names are not present.

The R version, package versions and operating sytem used in the most recent successful compilation of the report are printed under the heading "Session info" at the end
of the report file `Challenge_assignment_report.html'.

Case fatality estimates are by default rounded to whole percentage values. To present calculations with no rounding, the value of the `rounded' parameter in the calls
to the functions calculateCarefulCaseFatality and calculateCrudeCaseFatality may be changed to `FALSE'.

Updates to this script, and one example report that has been generated using the most recent version, may be found at https://github.com/ElishaBayode/Elisha_Jeremy_Challenge_Assignment.


