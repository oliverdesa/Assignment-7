# Name: Oliver De Sa
# SciNet username: tmp_odesa
# Description:
#   Driver script for Assignment 7

source('C:/Users/Oliver/MSC1090/assignment7/A7.Utilities.R')

args <- commandArgs(trailingOnly = TRUE)

# add defensive

CI.df <- angle.diff.CI(args, 1000, 2000)



