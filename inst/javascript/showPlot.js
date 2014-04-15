function showPlot(evt, img_link) {
	
	    // get a pointer to the parent HTML document
	    var doc = top.document;

	    // check if we already have a '_popup' element in the DOM;
	    // if we do, remove it
	    var old_img = doc.getElementById('_popup');
	    if (old_img) {
		doc.body.removeChild(old_img);

		// if the source of 'old_img' is the same as 'img_link',
		// then we've clicked on the same point twice, and we
		// probably want to hide that image without re-generating
		// a new one
		if (old_img.getAttribute("src") == img_link) {
		    return;
		}
	    }

	    var img = doc.createElement("img");

	    // set the src first so we can query the width, height
	    img.setAttribute("src", img_link);

	    // get the width, height of the image
	    var img_width = img.naturalWidth;
	    var img_height = img.naturalHeight;

	    // get the window dimensions
	    var window_width = top.window.innerWidth;
	    var window_height = top.window.innerHeight;

	    // this may need to be tweaked to get it to display
	    // in the correct position
	    img.setAttribute("style",
		"position: fixed;" +
		"left: " + (window_width/2 - img_width/2) + "px;" +
		"top: " + (window_height/2 - img_height/2) + "px;"
	    )

	    // set the id, src so it displays the right image
	    img.setAttribute("id", "_popup");
	    img.setAttribute("src", img_link);
	    img.setAttribute(
		"onclick",
		"document.body.removeChild( document.getElementById('_popup') );"
	    );

	    // append it to the bottom of the body
	    doc.body.appendChild(img);
	};
