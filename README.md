# matrixbooking

This repository is for the Matrixbooking Shiny app, which links room bookings to occupancy of those rooms in order to report on their usage.

The data for this app is stored in the app bucket `alpha-app-matrixbooking`. In addition, it primarily draws upon an Athena database, `matrixbooking_app_db`, which is created and updated through an airflow task. The code for this is here: https://github.com/moj-analytical-services/airflow-matrix-scraper

The code is split up into the following:

## app.r

This is the Shiny code for constructing the app.

The app is split into four panes:

### Data download

This is the interface for selecting a dataset and date range, as well as generating a Word report based on the selected data. The dataset comes from the list of Occupeye surveys, because there can be several surveys of the same building, so doing it by building (i.e. from a Matrixbooking point of view rather than Occupeye) would produce ambiguous (and double-counted) results.

### By room

This has a number of charts and tables showing the usage patterns of a specified room.

### By building

This shows a similar set of charts and tables, but looking over the whole building instead. There's currently just a filter for the room size, though there are plans for more filters.

### Top users

This has some anonymised counts of the top users, in three views:

1. Top bookers by number of hours booked
2. Top bookers of out of hours, with separate dropdowns for what constitutes "out-of-hours" (since it's ok to be booking rooms at 8am, but less so at midnight)

## data_cleaning_functions.R

This is where data cleaning functions. These functions manipulate the data pulled from 

## charting_functions.R

These are functions which create the charts and tables in the app, or are supporting functions for these charts. Several of these functions are vestigial, particularly those involving util_cat, which is the metric used in the Occupeye app but isn't relevant here.

Some of the functions take the Bookings data as input. However, most of the functions take as input a standard dataset called "joined_observations". This is a combination of the Occupeye data, which records occupancy in 10-minute snapshots, and the bookings data which is converted into 10 minute snapshots to match the Occupeye data.

## Athena_queries.R

These are getter methods that get the datasets used in the app from the app database, based on the selected occupeye survey and the selected date range where relevant.