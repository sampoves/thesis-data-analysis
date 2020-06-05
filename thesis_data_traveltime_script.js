
// JavaScript for the travel time comparison app of my thesis results

// "Parking of private cars and spatial accessibility in Helsinki Capital Region" 
// by Sampo Vesanen, 4.6.2020


// use this counter to hopefully detect flow of code
function Counter() {
	this.n = 0,
	this.add = function() {
		this.n++; 
	}
}
columnChangerCount = new Counter();


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



// ------------------------------------------------- //
// Send YKR_ID information to numeric input on click //
// ------------------------------------------------- //

// Add click listener for all ids that contain "svg_". Then retrieve the tooltip contents
// and fetch the current ykr_id. Send that to YKR_ID number input field. 
// Wrap this inside a clunky timeout function because couldn't figure out another way to 
// automatically establish these features as these elements are not finished at shiny:idle
// or document ready.
// Additionally, make this happen each time Shiny reports "idle" state.
$(document).on('shiny:idle', function(event) {
	
	setTimeout(function() {
		$('polygon').click(function() {
			
			// On click, change value in numeric input
			var tooltip_content = $(this)[0].attributes.title.textContent;
			var clean_ykr = clean(tooltip_content);
			$("#ykrid").val(clean_ykr).change();
			
			// Next, animate the numeric input to notify user
			var flash_elem = document.getElementById("ykr-flash");
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
	}, 3000);
});

// this function only preserved integers of length 7 from the character string that is 
// the tooltip inside the clicked polygon element
function clean(str) {
	return parseInt(str.match(/\d{7}/g));
}


// --------------------------------- //
// Colorise active column in tooltip //
// --------------------------------- //

// This is carried out in a rather complicated manner. First, define two functions
// which carry out the actual CSS property changes. Then, define a function that
// runs the CSS property changes according to the current value in input$ykrid.
// Finally, we have a document listening function that checks from the object
// columnChangerCount if this is the first time the map is opened. Then it either
// runs the first time option or any other time option.

// Highlight table column that is currently active in map fill
// With the help from: https://stackoverflow.com/a/18487765/9455395
$.fn.colorColumn = function(column) {
	return this.find('tr').map(function() {
		return $(this).find('td').eq(column).css({
			'font-weight': 'bold',
			'background-color': '#008a27'
		});
    }).get();
};
// Revert selected column CSS to default
$.fn.revertColumn = function(column) {
	return this.find('tr').map(function() {
		return $(this).find('td').eq(column).removeAttr('style');
    }).get();
};

function columnColorize() {
	// Revert column css before assigning new css properties
	var i;
	for (i = 0; i <= 3; i++) {
		$('table').revertColumn(i);
	}
	$('.selected').removeClass('selected');
	
	// Allow a small time window for revertColumn() to finish before 
	// running colorColumn()
	setTimeout(function() {
		// Find current drop-down menu value
		var attr_val = $('.item').attr('data-value');
		
		// Apply CSS to correct table columns
		$('[id^="svg_"]').hover(function() {
			if(attr_val === "ttm18_r_avg") {
				$('table').colorColumn(1);
				$('#ttm-avg').addClass("selected");
			} else if (attr_val === "ttm18_m_avg") {
				$('table').colorColumn(2);
				$('#ttm-avg').addClass("selected");
			} else if (attr_val === "ttm18_sl_avg") {
				$('table').colorColumn(3);
				$('#ttm-avg').addClass("selected");
			} else if (attr_val === "ttm18_r_t") {
				$('table').colorColumn(1);
				$('#ttm-car').addClass("selected");
			} else if (attr_val === "ttm18_m_t") {
				$('table').colorColumn(2);
				$('#ttm-car').addClass("selected");
			} else if (attr_val === "ttm18_sl_t") {
				$('table').colorColumn(3);
				$('#ttm-car').addClass("selected");
			}
		});
	}, 250);
}

// Listen to the state of the app and run columnColorize() according
// to that
$(document).ready(function() {
	if (columnChangerCount.n === 0) {
		$(document).on('shiny:idle', function(event) {
			setTimeout(function() {
				columnColorize();
				columnChangerCount.add();
			}, 3000);
		});
	} else {
		$('#grid').on('shiny:value', function(event) {
			columnColorize();
		});
	}
});


// ------------------------------------ //
// Attempt to make SVG animation happen //
// ----------------------------------- -//

// The following SVG animation on click is commented because of performance reasons.
// This SVG animation works with SVG.js, but is taxing on the performance. On click,
// highlight clicked ykrid. The transform almost works, but is commented for now. It needs
// front() accompanied with it, and that causes all sorts of trouble in the visuals.
//$(document).one('shiny:idle', function(event) {
//	
//	setTimeout(function() {
//		
//		$('polygon').click(function() {
//			var anim_elem = SVG($(this)[0]);
//			anim_elem.animate({
//				duration: 400,
//				delay: 0,
//				when: 'now',
//				swing: true,
//				times: 2,
//				wait: 200
//			}).attr({fill: '#ffffff'});//.transform({scale: 1.5});
//		});
//	}, 4000);
//});