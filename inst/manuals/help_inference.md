#  Inference Panel (Multi-Model Image Inference)

## Overview

This document is a **user manual** for the **Inference Panel**, a Shiny module that performs multi-model inference on `.tif` images.  
The module allows users to select an image directory, set inference parameters (e.g., IoU threshold), and execute inference using multiple YOLO-based object detection models.

The module integrates **R**, **Python**, and **Ultralytics YOLO** via the `reticulate` package.

---

# Purpose of the Panel

The **Inference Panel** provides an interactive interface for:
1. Selecting a subfolder containing `.tif` images (usually generated from AVI files).
2. Setting output parameters for inference results.
3. Running inference using multiple YOLO models packaged within the R environment.
4. Viewing log messages and results interactively.

This panel is part of a larger **flySurvivalApp** framework but can also operate independently.

---

#  Panel Layout

The user interface (UI) is composed of two main sections: a **Sidebar Panel** and a **Main Panel**.

## Sidebar Panel

The sidebar provides user controls:

| Element | Type | Description |
|----------|------|-------------|
| **Choose Image Subfolder** | Folder selector | Opens a file system dialog to select the folder containing `.tif` images. |
| **Output File Name** | Text input | Name for the output inference file (without `.txt` extension). this file contains the orientated bounding box of every detected pupa in the dataset |
| **IoU Threshold** | Numeric input | Sets the Intersection over Union threshold for merging overlapping pupa predictions in a single frame. Default = `0.5`. |
| **Run Inference** | Action button | Starts the inference process. |
