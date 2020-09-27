
// JavaScript for the travel time comparison app of my thesis results

// "Parking of private cars and spatial accessibility in Helsinki Capital Region" 
// by Sampo Vesanen, 28.9.2020



// Trigger application's first comparison map calculation
$(document).one('shiny:idle', function(event) {
	$('#calcZip').click();
});

// use this counter to hopefully detect flow of code. It's quite useless, frankly.
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
		onoff[i].style.marginBottom = '6px';
	};
});



// ------------------------------------------------------------------------------------- //
// Send zipcode information to numeric input on click and handle tooltip overflow events //
// ------------------------------------------------------------------------------------- //

// Add click listener for all ids that contain "svg_". Then retrieve the tooltip contents
// and fetch the current zipcode. Send that to zipcode number input field. 
// Wrap this inside a clunky timeout function because couldn't figure out another way to 
// automatically establish these features as these elements are not finished at shiny:idle
// or document ready.
// Additionally, make this happen each time Shiny reports "idle" state.

function isOverflown(element) {
	return element.scrollHeight > element.clientHeight || element.scrollWidth > element.clientWidth;
}

$(document).on('shiny:idle', function(event) {
	
	setTimeout(function() {
		$('polygon[id^="svg_"]').click(function() {
			
			// On click, change value in numeric input
			var tooltip_content = $(this)[0].attributes.title.textContent;
			var clean_zip = clean(tooltip_content);
			$('#zipcode').val(clean_zip).change();
			
			// Next, animate the numeric input to notify user
			var flash_elem = document.getElementById("zip-flash");
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
		
		// If #app-tooltip is overflown, widen the div
		$('polygon[id^="svg_"]').hover(function() {
			var tooltip = document.getElementById('app-tooltip');
			
			if(isOverflown(tooltip)) {
				tooltip.style.width = '234px';
			};
		});
		
		// Handle mouse wheel scrolling of #app-tooltip
		$('polygon[id^="svg_"]').on('mousewheel DOMMouseScroll', function (e) {
			var e0 = e.originalEvent,
				delta = e0.wheelDelta || -e0.detail;
			
			// Use the delta value of the current svg to scroll #app-tooltip. This produces
			// an uneven scrolling experience, but it works. 
			var scrollThis = document.getElementById('app-tooltip');
			scrollThis.scrollTop += (delta < 0 ? 1 : -1) * 120;
			
			// Prevent normal mousewheel behaviour
			e.preventDefault();
		});
	}, 3000);
});

// this function only preserves integers of length 5 from the character string that is 
// the tooltip inside the clicked polygon element. A postal code is included three times 
// in the tooltip, and we want the second one, therefore return [1].
function clean(str) {
	return str.match(/\d{5}/g)[1];
}



// --------------------------------- //
// Colorise active column in tooltip //
// --------------------------------- //
// 
// This is carried out in a rather complicated manner. First, define two functions
// which carry out the actual CSS property changes. Then, define a function that
// runs the CSS property changes according to the current value in input$zipcode.
// Finally, we have a document listening function that checks from the object
// columnChangerCount if this is the first time the map is opened. Then it either
// runs the first time option or any other time option.
//
// Highlight table column that is currently active in map fill
// With the help from: https://stackoverflow.com/a/18487765/9455395
// use :not selector to exempt td's of #legend-table from this functionality
$.fn.colorColumn = function(column) {
	return this.find('tr').map(function() {
		return $(this).find('td:not(.legend-td)').eq(column).css({
			'font-weight': 'bold',
			'background-color': '#616161'
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
		$(this).find('i.icon').remove();
		
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
			
			// Color columns
			if (attr_split === 'r') {
				$('table.tg').colorColumn(1);
				$('table.tg').addIconsToCompare(1);
			} else if (attr_split === 'm') {
				$('table.tg').colorColumn(2);
				$('table.tg').addIconsToCompare(2);
			} else if (attr_split === 'all') {
				$('table.tg').colorColumn(3);
				$('table.tg').addIconsToCompare(3);
			}
			
			// Color rows
			if ('ttm18_r_avg,ttm18_m_avg,ttm18_all_avg'.includes(attr_val)) {
				$('#ttm-avg').addClass("selected");
			} else if ('ttm18_r_drivetime,ttm18_m_drivetime,ttm18_all_drivetime'.includes(attr_val)) {
				$('#ttm-drivetime').addClass("selected");
			} else if ('ttm18_r_pct,ttm18_m_pct,ttm18_all_pct'.includes(attr_val)) {
				$('#ttm-pct').addClass("selected");
				
			} else if ('msc_r_sfp,msc_m_sfp,msc_all_sfp'.includes(attr_val)) {
				$('#thesis-sfp').addClass("selected");
			} else if ('msc_r_wtd,msc_m_wtd,msc_all_wtd'.includes(attr_val)) {
				$('#thesis-wtd').addClass("selected");
			} else if ('msc_r_drivetime,msc_m_drivetime,msc_all_drivetime'.includes(attr_val)) {
				$('#thesis-drivetime').addClass("selected");
			} else if ('msc_r_pct,msc_m_pct,msc_all_pct'.includes(attr_val)) {
				$('#thesis-pct').addClass("selected");
				
			} else if ('compare_r_sfp,compare_m_sfp,compare_all_sfp'.includes(attr_val)) {
				$('#compare-sfp').addClass("selected");
			} else if ('compare_r_wtd,compare_m_wtd,compare_all_wtd'.includes(attr_val)) {
				$('#compare-wtd').addClass("selected");
			} else if ('compare_r_drivetime,compare_m_drivetime,compare_all_drivetime'.includes(attr_val)) {
				$('#compare-drivetime').addClass("selected");
			} else if ('compare_r_pct,compare_m_pct,compare_all_pct'.includes(attr_val)) {
				$('#compare-pct').addClass("selected");
			}
			
			// A clumsy colouring of the active visualising cell value
			var thisSelectedRow = $('.tg-cell').parents('.selected');
			
			if (attr_split === "r") {
				$('.tg-cell').parents('.selected').children()[1].style.backgroundColor = '#008a27';
			} else if (attr_split === "m") {
				$('.tg-cell').parents('.selected').children()[2].style.backgroundColor = '#008a27';
			} else if (attr_split === "all") {
				$('.tg-cell').parents('.selected').children()[3].style.backgroundColor = '#008a27';
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



// Add icons to dropdown menu for user's aid or confusion. In this implementation we need 
// to reinitialise ids every time the dropdown menu opens
$(document).one('shiny:idle', function(event) {
	
	// Define tooltip texts for drop down menu items
	var symbology_arr = ['Travel Time Matrix 2018, rush hour, total travel chain', 
	'Travel Time Matrix 2018, midday, total travel chain', 
	'Travel Time Matrix 2018, all values, total travel chain', 
	'Travel Time Matrix 2018, rush hour, driving time', 
	'Travel Time Matrix 2018, midday, driving time', 
	'Travel Time Matrix 2018, all values, driving time', 
	'Travel Time Matrix 2018, rush hour, percentage', 
	'Travel Time Matrix 2018, midday, percentage', 
	'Travel Time Matrix 2018, all values, percentage', 
	'Thesis survey data, rush hour, searching for parking',
	'Thesis survey data, midday, searching for parking',
	'Thesis survey data, all values, searching for parking',
	'Thesis survey data, rush hour, walking to destination',
	'Thesis survey data, midday, walking to destination',
	'Thesis survey data, all values, walking to destination',
	'Thesis survey data, rush hour, driving time',
	'Thesis survey data, midday, driving time',
	'Thesis survey data, all values, driving time',
	'Thesis survey data, rush hour, percentage',
	'Thesis survey data, midday, percentage',
	'Thesis survey data, all values, percentage',
	'Compare datasets, rush hour, searching for parking',
	'Compare datasets, midday, searching for parking',
	'Compare datasets, all values, searching for parking',
	'Compare datasets, rush hour, walking to destination',
	'Compare datasets, midday, walking to destination',
	'Compare datasets, all values, walking to destination',
	'Compare datasets, rush hour, driving time',
	'Compare datasets, midday, driving time',
	'Compare datasets, all values, driving time',
	'Compare datasets, rush hour, percentage',
	'Compare datasets, midday, percentage',
	'Compare datasets, all values, percentage'];
	
	// Add tooltip text for comparable colors drop down menu
	var locked_arr = ['Feature is disabled. A change in any parameter triggers recalculation of\nclass intervals.', 
	'Feature is in use for rush hour-midday-all values trios. Class\ninterval recalculation will trigger if origin postal code area or\nparameter type, for example from compare_drivetime to\nttm18_avg, is changed.', 
	'Feature is in use for all postal code areas and rush hour-midday-all values\ntrios. Class interval recalculation will only trigger when parameter type is\nchanged, for example from ttm_drivetime to msc_pct.'];
	
	// Disable mobile keyboard on selectize.js dropdown menus
	$('.selectize-input input').attr('readonly', 'readonly');
	
	$('.selectize-input').on('click', function (e) {
		// Find only the first dropdown menu, the symbology selection
		var drop_lbls = $('.selectize-dropdown').first().find('.option');
		
		// Add ids for dropdown items
		for(var i = 0; i < drop_lbls.length; i++) {
			drop_lbls[i].setAttribute("id", "drop_" + i);
		};
		
		// add coloured bars to the open dropdown menu
		for(var i = 0; i < drop_lbls.length; i++) {
			
			var this_id = '#drop_' + i;

			// these attributes add tooltips to dropdown items.
			$(this_id).attr('data-placement', 'right');
			$(this_id).attr('title', symbology_arr[i]);
			
			if (i >= 0 && i <= 2) {
				$(this_id).prepend("<i class='dropbar'></i>");
			} else if (i >= 3 && i <= 8 || i >= 15 && i <= 20 || i >= 27 && i <= 32) {
				$(this_id).prepend("<i class='dropbar dd-drive-pct'></i>");
			} else if (i >= 9 && i <= 14 || i >= 21 && i <= 26) {
				$(this_id).prepend("<i class='dropbar dd-sfp-wtd'></i>");
			}
		};
		
		// add icons to the open dropdown menu in the same fashion as above
		var optgroup_lbls = $('.selectize-dropdown').find('.optgroup-header');
		for(var i = 0; i < optgroup_lbls.length; i++) {
			optgroup_lbls[i].setAttribute("id", "optlbl_" + i);
		};
		// "Visualise data (Equal interval)"
		$('#optlbl_0').prepend("<i class='icon database-grey'></i>");
		$('#optlbl_1').prepend("<i class='icon poll-grey'></i>");
		$('#optlbl_2').prepend("<i class='icon exchange-grey'></i>");
		// "Color scheme"
		// This is buggy right now, fill icons do not appear on first open of dropdown
		// menu
		$('#optlbl_3').prepend("<i class='icon fill-grey'></i>");
		$('#optlbl_4').prepend("<i class='icon fill-grey'></i>");
	});
	
	// Add helping tooltips to locked class intervals dropdown menu
	$('.selectize-input').eq(2).on('click', function (e) {
		var locked_lbls = $('.selectize-dropdown').eq(2).find('.option');
		
		// Add ids for dropdown items
		for(var i = 0; i < locked_lbls.length; i++) {
			locked_lbls[i].setAttribute("id", "lock_" + i);
		};
		
		// add tooltip texts to locked class intervals dropdown menu
		for(var i = 0; i < locked_lbls.length; i++) {
			
			var this_id = '#lock_' + i;
			
			// these attributes add tooltips to dropdown items.
			$(this_id).attr('data-placement', 'right');
			$(this_id).attr('title', locked_arr[i]);
		};
	});
});



// ------------------------------------------------------ //
// Message when fill_column does not respond to an origin //
// ------------------------------------------------------ //
$(document).on('shiny:idle', function(event) {
	
	var fillcol = $('#fill_column').val();
	if ('msc_r_sfp, msc_m_sfp, msc_all_sfp, msc_r_wtd, msc_m_wtd, msc_all_wtd, compare_r_sfp, compare_m_sfp, compare_all_sfp, compare_r_wtd, compare_m_wtd, compare_all_wtd'.includes(fillcol)) {
		$('.travelchain').addClass('fillcol-warning');
	} else {
		$('.travelchain').removeClass('fillcol-warning');
	}
});



// --------------------------- //
// Multi-part SVG highlighting //
// --------------------------- //

// CSS has been defined so that highlighting works for onepart polygons. To reduce
// confusion about multipart polygons (zipcode areas), run this code.
$(document).on('shiny:idle', function(event) {
	
	// Give SVG polygons attribute "name"
	setTimeout(function() {
		var pols = $('polygon[id^="svg_"]');

		for(var i = 0; i < pols.length; i++) {
			try {
				var cleantitle = clean(pols[i].attributes.title.textContent);
				pols[i].setAttribute("name", cleantitle);
			} catch (error) {
				continue;
			}
		};
	}, 3000);
	
	// The predetermined postal code areas "arr" are multipart. Add events for these 
	// postal code areas.
	setTimeout(function() {
		
		var arr = ["00190", "00200", "00250", "00330", "00340", "00570", "00890", "02100", "02380"];
		
		arr.forEach(function (value) {
			var $var = $('polygon[name="' + value +'"]');
			$var.mouseenter(function() {
				$var.css('fill', '#8950a1');
			}).mouseleave(function() {
				$var.removeAttr('style');
			});
		});
	}, 3100);
});
