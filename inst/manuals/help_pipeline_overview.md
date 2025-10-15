How the general pipeline works

This software is designed to work in conjunction with the terrariums and produces 
an excel file containing every detected pupariation in a video recorded by one. 

General pipeline steps 

1: The video is converted from avi to individual frames by the AVI->TIFF

2: The inference tab to runs a neural network inference on the individual 
   frames detecting each appearance of a pupa in a inference results file 

3: The Data and Images tab is used to load the avi frames and the inference
   results file so that the detected pupa and can be overlaid on the images
   for checking the quality of detections  
   
4: The next step is the most complex and uses both the browse inferences and 
   individual pupa browser and so is broken down in substeps

4.1: The first step is using the Load/analyse input file to organised the run
     of creating ids for each individual pupa and propagating them. The merging
     IoU threshold is the intersection over union required to merge the 
     orientated bounding boxes over the frames. The minimum appearances per ID
     filter allows for the  removal  of potentially spurious  ids by filtering 
     out IDs  of pupa that appear less than that number of times. It may need to
     be increased from the default to remove objects misidentified as pupa
     
4.2: The manual step now is the validation of the bounding boxes and checking 
     the whether the pupa have been detected for the first time correctly. This 
     uses a combination of the browse panel and the single pupa panel to remove
     false detentions using their id number and the box to make sure the correct
     point of pupariation is used for the export with the move first appearance 
     button and slider. 
     
5:  Once you are happy with the pupariations being at the correct timing you 
    then should add your meta data to the export tab, use the slider to split 
    the left and right side then click export to send the export to the output  
    folder in your directory.
     
     
