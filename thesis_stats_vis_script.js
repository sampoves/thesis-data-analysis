// This JS function checks the state of the display parameter of inputted div "shrink_me"
// and edits the value accordingly. Also change the eye icon of the inputted "parent_div"
// parameter.
function show_hide(shrink_me, parent_div) {
	var elem_to_hide = document.getElementById(shrink_me);
	var parent_div = '#' + parent_div;

	if(elem_to_hide.style.display === 'none') {
		elem_to_hide.style.display = 'block';
		$(parent_div).find('i.icon.eye').toggleClass('eye eyeslash');
		$(parent_div).find('i.icon.eyeslash')[0].setAttribute('title', 'Hide element');
	} else {
		elem_to_hide.style.display = 'none';
		$(parent_div).find('i.icon.eyeslash').toggleClass('eyeslash eye');
		$(parent_div).find('i.icon.eye')[0].setAttribute('title', 'Show element');
	}
};

// This jQuery function listens to anchor link clicking. With this function I aimed to
// make repeated clicking possible, which was not with :target selectors I used earlier.
// In short, this detects with the amount of clicks if we fire a delayed highlight or
// an immediate one. A bit hacky setTimeout() is employed to remove the delayed highlight.
// leave console.log()'s in to illustrate the behaviour of this function.
$(function() {
	
	var timesClicked = 0;
	
	$("a").on("click", function() {
		timesClicked++;
		var dest_id = $(this)[0].hash;
		var dest = dest_id.replace('#', '');
		element = document.getElementById(dest);
		
		if(timesClicked === 1) {
			console.log(`${dest_id} timesclicked: ${timesClicked}`);			
			$(dest_id).addClass('animate');
			
			ticktock = function() {
				timeout_val = setTimeout(function() {
					$(dest_id).removeClass('animate');
					// The next line removes the immediate animation from the entire
					// document. Without this fast clicking around the document ends
					// with the last div id having a lingering animation class.
					$('.animate-immediately').removeClass('animate-immediately');
					timesClicked = 0;
					console.log("end");
				}, 2000);
			}
			ticktock();
		}
		
		// Detect a click event while the class .animate is present in dest_id
		if (timesClicked > 1) {
			console.log(`timesclicked: ${timesClicked}, interrupt start`);
			clearTimeout(timeout_val);
			element.classList.remove('animate');
			element.classList.remove('animate-immediately');
			
			// Trigger a DOM reflow with .offsetWidth. A simple elem.remove().add() will 
			// not work to interrupt a CSS animation and run it again. See:
			// https://css-tricks.com/restart-css-animation/
			void element.offsetWidth;
			element.classList.add('animate-immediately');
			ticktock();
		}
    });
});