function show_hide(divname, shrink_div) {
	var this_elem = document.getElementById(divname);
	var shrink_name = '#' + shrink_div;

	if(this_elem.style.display === 'none') {
		this_elem.style.display = 'block';
		$(shrink_name).find('i.icon.eye').toggleClass('eye eyeslash');
		$(shrink_name).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide element');
	} else {
		this_elem.style.display = 'none';
		$(shrink_name).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(shrink_name).find('i.icon.eye')[0].setAttribute('title', 'Show element');
	}
};

// We need to listen to when Shiny has finished loading. Otherwise running JavaScript
// will not work. Please see this link for more details: 
// https://shiny.rstudio.com/articles/js-events.html
$(document).on('shiny:connected', function(event) {
	
	var coll = document.getElementsByClassName('collapsible');
	var i;
	
	// The first time Shiny app is connected and idle, set collapsibles open by default.
	// maxHeight 2000px is quite hacky, but I couldn't find a better solution.
	$(document).one('shiny:idle', function(event) {
		
		for (i = 0; i < coll.length; i++) {
			coll[i].classList.toggle('active');
			var content = coll[i].nextElementSibling;
			content.style.maxHeight = '2000px';
			coll[i].setAttribute('title', 'Hide element');
		};
	});

	// Collapsible element event listener
	for (i = 0; i < coll.length; i++) {
		
		coll[i].addEventListener('click', function() {
			
			this.classList.toggle('active');
			var content = this.nextElementSibling;
			
			if (content.style.maxHeight) {
				content.style.maxHeight = null;
				this.setAttribute('title', 'Show element');
			} else {
				content.style.maxHeight = content.scrollHeight + 'px';
				this.setAttribute('title', 'Hide element');
			}
		});
	}
	
	// When clicking icons, prevent firing up the minimising div underneath it
	var icons = document.getElementsByClassName('icon');
	
	for (i = 0; i < icons.length; i++) {
		icons[i].addEventListener('click', function(e) {
			e.stopPropagation();
		});
	}
});

