
// JavaScript for the travel time comparison app of my thesis results

// "Parking of private cars and spatial accessibility in Helsinki Capital Region" 
// by Sampo Vesanen, 30.5.2020



// Insert sidebar hide/show button to col-sm-3, the div that contains the sidebar. Add
// the button after the actual sidebar div. The button appears on the top right corner
// of the sidebar.
$(function() {
	$('.well').after("<div class='hidesidebar'><button id='showhidebutton' onclick='show_hide_sb()'><i class='icon eyeslash' title='Hide sidebar'></i></button></div>");
});

// Specified function to hide/show sidebar
function show_hide_sb() {
	var elem_to_hide = document.getElementById('sidebar');
	var showhidebutton = document.getElementById('showhidebutton');

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(showhidebutton).find('i.icon.eye').toggleClass('eye eyeslash');
		$(showhidebutton).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide sidebar');
	} else {
		elem_to_hide.style.display = 'none';
		$(showhidebutton).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(showhidebutton).find('i.icon.eye')[0].setAttribute('title', 'Show sidebar');
	}
};

// Attribute and CSS operations in sidebar
$(document).ready(function() {
	// find all on-off switches in Layer options and reduce their
	// margin-bottoms.
	var onoff = $(".onoff-container").find(".form-group.shiny-input-container");
	for(var i = 0; i < onoff.length; i++) {
		onoff[i].style.marginBottom = "6px";
	};
});

// Add click listener for all ids that contain "svg_". Then retrieve the tooltip contents
// and fetch the current ykr_id. Send that to YKR_ID number input field. 
// Wrap this inside a clunky timeout function because couldn't figure out another way to 
// automatically establish these features as these elements are not finished at shiny:idle
// or document ready.
$(document).one('shiny:idle', function(event) {
	
	setTimeout(function() {
		$('polygon').click(function() {
			
			// On click, change value in numeric input
			var tooltip_content = $(this)[0].attributes.title.textContent;
			var clean_ykr = clean(tooltip_content);
			$("#ykrid").val(clean_ykr).change();
			
			// Next, animate the numeric input to notify user
			flash_elem = document.getElementById("ykr-flash");
			flash_elem.classList.add('animate');
		
			//run after 0.8 seconds. 0.05 seconds longer than animation
			ticktock = function() {
				timeout_val = setTimeout(function() {
					flash_elem.classList.remove('animate');
					$('.animate').removeClass('animate');
				}, 800);
			}
			ticktock();
		});
	}, 4000);
});

// this function only preserved integers of length 7 from the character string that is 
// the tooltip inside the clicked polygon element
function clean(str) {
	return parseInt(str.match(/\d{7}/g));
}