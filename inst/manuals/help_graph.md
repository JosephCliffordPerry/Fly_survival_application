# Export Panel User Manual

## Overview

The `graph_panel()` **Shiny module** provides an interface to **format, visualize, and export** first pupariation events from the tracked dataset.  
It allows users to:

- Preview frames and mark the **chamber split**.  
- Enter **experimental metadata** for left and right chambers.  
- Visualize **cumulative pupariation events** over time.  
- Export a formatted **Excel file** with all relevant data.

---

 Sidebar Components
Control	|Type|Purpose
Start Date |Date Input|	Specify the start date for event timing.
Start Time | Text Input|	Specify start time in HH:MM format.
Chamber Split | Slider Input|	Set x-coordinate separating left and right chambers.
Left Side Info | Group of Inputs	|Treatment, Dose, Dose Unit, Genotype, Replicate for Left chamber.
Right Side Info	| Group of Inputs|	Same as Left chamber for Right side.
Export Excel (export_excel)|	Action Button|	Export formatted dataset to Excel file.

 Main Panel Components
Output	Type	Description
Frame Preview (frame_plot2)	Plot Output	Displays first frame with chamber split line overlaid.
Cumulative First Pupariations (frame_cumsum)	Plot Output	Shows stepwise cumulative pupariation events over time for Left and Right chambers.

 Workflow
Step 1: Set Experiment Metadata
Enter start date and time.

Adjust chamber split to separate left and right regions.

Fill in Left and Right side info: Treatment, Dose, Dose Unit, Genotype, Replicate.

Step 2: Preview Frames
The first frame is displayed with the chamber split line (blue dashed).

The slider adjusts automatically to the image width.

Step 3: Visualize Cumulative Pupariations
The panel calculates first pupariation per ID.

Determines side (Left/Right) based on average x-coordinates of bounding box.

Computes datetime of each event using start date/time and 5-min intervals per frame.

Cumulative events are plotted:

Color	Side
Red / default ggplot color	Left
Blue / default ggplot color	Right

Plot shows stepwise cumulative counts over time.

Step 4: Export Excel
Click Export Excel to save data to:

Excel contains two sheets: Left and Right, with columns:

Column	Description
Date	Formatted as DD.MM.YYYY
Time	Formatted as HH:MM
Event	Always 1 (represents pupariation occurrence)
Treatment	From metadata input
Dose	From metadata input
Dose_unit	From metadata input
Genotype	From metadata input
Replicate	From metadata input

Step 5: Notes on Timing Calculation
Frame intervals are assumed to be 5 minutes per frame.

Summary
The Export Panel enables users to prepare and save experiment-ready datasets from tracking data, including:

Frame preview for checking chamber alignment

Metadata annotation per chamber

Cumulative pupariation plots

Formatted Excel export ready for downstream analysis
