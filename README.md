Body Composition Trend R Functions
==================================

This is a collection of scripts I use with my FitBit Aria scale to calculate
linear regressions of weight, lean mass, fat mass, and body fat percentage
over time.

There are also some functions that pull in data from myfitnesspal.com and join
it to the data from the scale. These are there in hopes that I can find some
model to predict lean mass change and fat mass change in the future as a
function of food intake in the present. So far, surprisingly, not even net or
total calories have a high correlation coefficient with any of the lags I have
tried. I still believe there is a model that will fit will, but I haven't
found it yet.

These scripts assume there is a SQLite database in the R working directory and
have an opinion about its schema. The SQL DDL statements for that schema and
the code that populates the database with the scale data from fitbit.com are
part of a Rails app that I have not yet pushed to github.
