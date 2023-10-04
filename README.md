## SimpleDelphiTest01

Compile + Run from Delphi IDE

FMX for eventual portability

Note that const datadir sets the main data directory to ../../data/ which should make this run directly from the IDE

There are eight images in the cards data/cards subdir (WOTC allow re-use so close to a free license)

Most of the code is just boilerplate to set up a CGE Window etc

The last two functions/procedures are the ones causing the issues

MaskedImage applies a mask to a card image - end result is same image with rounded corners from the jpg source

MakeCard switches out the Image_Front texture from data/static/card.x3d replacing it with a constructed image via MaskedImage

The CGE window's onclick handler calls MakeCard. It SHOULD cycle thru eight distinct images. The Dragon card you get on load should never be seen after the first click (it's not in the data/cards dir, only in data/static as a initial image placeholder)

As provided the image is changed to the image we're trying to get to then we get that one BUT NOT with the rounded corners returned from MaskedImage (it's subtle - no corners)

Remove the comments on line 334 and UpdateUrl becomes an empty string which has the effect of visibly doing nothing when MakeCard is called

If UpdateUrl is changed to some random junk string an exception will be raised if run in the debugger

