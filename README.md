# Input-Output-Dashboard

This is a dashboard that I made for a work project that took place from the end of 2021 to early 2022. This was one of my first shiny apps, so lots of best-practices were not followed. I plan on updating the code as time permits. 

# Motivation: 

As my workplace assembles EV harnesses, we are constantly recieving components, performing assemblies, and then shipping completed/reworked assemblies. However, problems arise when production slows or when clients don't pick up their finished assemblies on-time as the warehouse becomes accumulating inventory, and thus warehouse space. In this application, the goal was to have convenient visibility on the rate of components received vs the rate of final assemblies produced/shipped. Currently at my workplace we have a private production control software that tracks hourly production data by part number, shipping data, and current inventory by part number all in separate modules. However, it becomes very tedious for our management team to cycle through different webpages to review and compare the data on a daily basis. 

Please keep in mind that for this version of the dashboard I've changed actual part numbers (PNs) to reflect generic part numbers (PN 1, PN 2, etc.) for privacy reasons. In the official shiny application, I've established an ODBC connection to our microsoft SQL server so that that the dashboard will always present up-to-date data by executing queries. However in this version, I exported all of the queries required into csv files as I'm unable to make our up-to-date data public. Therefore, this version does not update in real time unfortunately. 
