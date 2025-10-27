
#AVI → TIFF Conversion Panel


## Overview

This document serves as a **user manual** for the **AVI → TIFF Conversion Panel**, a Shiny module that allows users to convert `.avi` video files into `.tiff` image frame sequences.  

The panel provides a **graphical interface** for selecting folders containing `.avi` files, running conversions using **FFmpeg**, and viewing conversion logs in real time.



#  Purpose of the Panel

The **AVI → TIFF Conversion Panel** provides an easy-to-use interface to:
1. Browse and select a folder containing `.avi` files.
2. Execute a conversion process that extracts each frame as `.tiff` images.
3. Display progress and logs during conversion.

---

#  Panel Layout

The interface is divided into two main sections: **Sidebar** and **Main Panel**.

## Sidebar Panel

The sidebar includes:
- **Folder Selector:**  
  `Choose AVI Folder` — Opens a system dialog for selecting the directory containing `.avi` files.
  
- **Run Button:**  
  `Run Conversion` — Starts the AVI → TIFF conversion process.
