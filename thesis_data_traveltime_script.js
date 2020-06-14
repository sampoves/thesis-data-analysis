
// JavaScript for the travel time comparison app of my thesis results

// "Parking of private cars and spatial accessibility in Helsinki Capital Region" 
// by Sampo Vesanen, 14.6.2020



// Trigger application's first comparison map calculation
$(document).one('shiny:idle', function(event) {
	$("#calcYkr").click();
});

// use this counter to hopefully detect flow of code
function Counter() {
	this.n = 0,
	this.add = function() {
		this.n++; 
	}
}
columnChangerCount = new Counter();



// --------------------------------- //
// Comparison app information window //
// --------------------------------- //

// Establish jQuery UI window
$(function() {
	$('#abbr-info').dialog({
		autoOpen: false,
		closeOnEscape: true,
		dialogClass: 'dialog-dropshadow',
		width: 550,
		maxWidth: 600,
		maxHeight: 800,
		show: {
			effect: 'fade',
			duration: 300
		},
		hide: {
			effect: 'fade',
			duration: 300
		}
	});
});



// ----------------------- //
// SidebarPanel operations //
// ----------------------- //

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
		$('polygon[id^="svg_"]').click(function() {
			
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
// use :not selector to exempt td's of #legend-table from this functionality
$.fn.colorColumn = function(column) {
	return this.find('tr').map(function() {
		return $(this).find('td:not(.legend-td)').eq(column).css({
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
// Add icons to the comparison tables to signify the larger value
$.fn.addIconsToCompare = function(column) {
	return this.find('.compcol' + column).map(function() {

		var val = parseFloat($(this).text());
		
		// Attempt to prevent presence of multiple icons in one cell
		$(this).find("i.icon").remove();
		
		if (val < 1) {
			return $(this).append('<i class="icon database-comp"></i>');
		} else if (val > 1) {
			return $(this).append('<i class="icon poll-comp"></i>');
		} else if (val === 1) {
			return $(this).append('<i class="icon equals"></i>');
		}
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
		var attr_split = attr_val.split("_")[1];
		
		// Apply CSS to correct table columns and add icons to comparison tables
		$('polygon[id^="svg_"]').hover(function() {
			if (attr_split === "r") {
				$('table.tg').colorColumn(1);
				$('table.tg').addIconsToCompare(1);
			} else if (attr_split === "m") {
				$('table.tg').colorColumn(2);
				$('table.tg').addIconsToCompare(2);
			} else if (attr_split === "sl") {
				$('table.tg').colorColumn(3);
				$('table.tg').addIconsToCompare(3);
			}
			
			if ("ttm18_r_avg,ttm18_m_avg,ttm18_sl_avg".includes(attr_val)) {
				$('#ttm-avg').addClass("selected");
			} else if ("ttm18_r_drivetime,ttm18_m_drivetime,ttm18_sl_drivetime".includes(attr_val)) {
				$('#ttm-drivetime').addClass("selected");
			} else if ("ttm18_r_pct,ttm18_m_pct,ttm18_sl_pct".includes(attr_val)) {
				$('#ttm-pct').addClass("selected");
				
			} else if ("msc_r_sfp,msc_m_sfp,msc_sl_sfp".includes(attr_val)) {
				$('#thesis-sfp').addClass("selected");
			} else if ("msc_r_wtd,msc_m_wtd,msc_sl_wtd".includes(attr_val)) {
				$('#thesis-wtd').addClass("selected");
			} else if ("msc_r_drivetime,msc_m_drivetime,msc_sl_drivetime".includes(attr_val)) {
				$('#thesis-drivetime').addClass("selected");
			} else if ("msc_r_pct,msc_m_pct,msc_sl_pct".includes(attr_val)) {
				$('#thesis-pct').addClass("selected");
				
			} else if ("compare_r_sfp,compare_m_sfp,compare_sl_sfp".includes(attr_val)) {
				$('#compare-sfp').addClass("selected");
			} else if ("compare_r_wtd,compare_m_wtd,compare_sl_wtd".includes(attr_val)) {
				$('#compare-wtd').addClass("selected");
			} else if ("compare_r_drivetime,compare_m_drivetime,compare_sl_drivetime".includes(attr_val)) {
				$('#compare-drivetime').addClass("selected");
			} else if ("compare_r_pct,compare_m_pct,compare_sl_pct".includes(attr_val)) {
				$('#compare-pct').addClass("selected");
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


// Add icons to dropdown menu for user's aid or confusion. Need to reinitialise ids every 
// time the dropdown menu opens
$(document).one('shiny:idle', function(event) {
	
	// Disable mobile keyboard on selectize.js dropdown menus
	$('.selectize-input input').attr('readonly','readonly');
	
	$(".selectize-input").on("click", function (e) {
		var drop_lbls = $(".selectize-dropdown").find(".option");
		
		// Add ids for dropdown items
		for(var i = 0; i < drop_lbls.length; i++) {
			drop_lbls[i].setAttribute("id", "drop_" + i);
		};
		
		// add coloured bars to the open dropdown menu
		for(var i = 0; i < drop_lbls.length; i++) {
			
			var this_id = "#drop_" + i;
			if (i >= 0 && i <= 2) {
				$(this_id).prepend("<i class='dropbar'></i>");
			} else if (i >= 3 && i <= 5) {
				$(this_id).prepend("<i class='dropbar dd-avg'></i>");
			} else if (i >= 6 && i <= 11 || i >= 18 && i <= 23 || i >= 30 && i <= 35) {
				$(this_id).prepend("<i class='dropbar dd-drive-pct'></i>");
			} else if (i >= 12 && i <= 17 || i >= 24 && i <= 29) {
				$(this_id).prepend("<i class='dropbar dd-sfp-wtd'></i>");
			}
		};
		
		// add icons to the open dropdown menu in the same fashion as above
		var optgroup_lbls = $(".selectize-dropdown").find(".optgroup-header");
		for(var i = 0; i < optgroup_lbls.length; i++) {
			optgroup_lbls[i].setAttribute("id", "optlbl_" + i);
		};
		$("#optlbl_0").prepend("<i class='icon database-grey'></i>");
		$("#optlbl_1").prepend("<i class='icon poll-grey'></i>");
		$("#optlbl_2").prepend("<i class='icon exchange-grey'></i>");
	});
});
