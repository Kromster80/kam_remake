/*)
//buuble box
function initInfoBox() {
	$(".bubbleInfo").each(function () {
		//get the link
		var link = $(this).children();
		$(this).append("<table class='popup'><tbody><tr><td id='topleft' class='corner'></td><td class='top'></td><td id='topright' class='corner'></td></tr><tr><td class='left'></td><td><table class='popup-contents'><tbody><tr><td><img src='"+ link[0].rel +"' alt='image preview' /></td></tr></tbody></table></td><td class='right'></td></tr><tr><td class='corner' id='bottomleft'></td><td class='bottom'><img width='30' height='29' alt='popup tail' src='img//bubble-tail2.png'/></td><td id='bottomright' class='corner'></td></tr></tbody></table>");
		// options
		var distance = 10;
		var time = 250;
		var hideDelay = 500;
		var hideDelayTimer = null;
		// tracker
		var beingShown = false;
		var shown = false;
		var trigger = $(".trigger", this);
		var popup = $(".popup", this).css("opacity", 0);
		// set the mouseover and mouseout on both element
		$([trigger.get(0), popup.get(0)]).mouseover(function () {
			// stops the hide event if we move from the trigger to the popup element
			if (hideDelayTimer) {
				clearTimeout(hideDelayTimer);
			}
			// don't trigger the animation again if we're being shown, or already visible
			if (beingShown || shown) {
				return;
			} else {
				beingShown = true;
				// reset position of popup box
				popup.css({
					top: -100,
					left: -33,
					display: "block" // brings the popup back in to view
				})
				// (we're using chaining on the popup) now animate it's opacity and position
				.animate({
					top: "-=" + distance + "px",
					opacity: 1
				}, time, "swing", function() {
					// once the animation is complete, set the tracker variables
					beingShown = false;
					shown = true;
				});
			}
		}).mouseout(function () {
			// reset the timer if we get fired again - avoids double animations
			if (hideDelayTimer) {
				clearTimeout(hideDelayTimer);
			}
			// store the timer so that it can be cleared in the mouseover if required
			hideDelayTimer = setTimeout(function () {
				hideDelayTimer = null;
				popup.animate({
					top: "-=" + distance + "px",
					opacity: 0
				}, time, "swing", function () {
					// once the animate is complete, set the tracker variables
					shown = false;
					// hide the popup entirely after the effect (opacity alone doesn't do the job)
					popup.css("display", "none");
				});
			}, hideDelay);
		});
	});
}
//bubble box
*/

/*function showpic() {
	$("a.prev").hover(function(hIn){
		// if no rel is specified, then no popup image is given - abort
		if ( this.rel === "" ) {
			return;
		}
		$("body").append("<div class='previewpic'><img src='"+ this.rel +"' alt='image preview' /></div>");
		$(".previewpic")
			.css("top",(hIn.pageY - 5) + "px")
			.css("left",(hIn.pageX + 5) + "px")
			.css("position","absolute")
			.fadeIn("fast");	// 5px offset in x- and -y-direction
    },
	function(){
		$(".previewpic").remove();
    });	
	$("a.prev").mousemove(function(hIn){
		// to have the image wandering with the mouse icon
		$("#previewpic")
			.css("top",(hIn.pageY - 5) + "px")
			.css("left",(hIn.pageX + 5) + "px");
	});			
};*/

/*
 * Url preview script 
 * powered by jQuery (http://www.jquery.com)
 * 
 * written by Alen Grakalic (http://cssglobe.com)
 * 
 * for more info visit http://cssglobe.com/post/1695/easiest-tooltip-and-image-preview-using-jquery
 *
 */
 
this.screenshotPreview = function(){	
	/* CONFIG */
		
		xOffset = 10;
		yOffset = 30;
		
		// these 2 variable determine popup's distance from the cursor
		// you might want to adjust to get the right result
		
	/* END CONFIG */
	$("a.screenshot").hover(function(e){
		this.t = this.title;
		this.title = "";	
		var c = (this.t != "") ? "<br/>" + this.t : "";
		$("body").append("<p id='screenshot'><img src='"+ this.rel +"' alt='url preview' />"+ c +"</p>");								 
		$("#screenshot")
			.css("top",(e.pageY - xOffset) + "px")
			.css("left",(e.pageX + yOffset) + "px")
			.fadeIn("fast");						
    },
	function(){
		this.title = this.t;	
		$("#screenshot").remove();
    });	
	$("a.screenshot").mousemove(function(e){
		$("#screenshot")
			.css("top",(e.pageY - xOffset) + "px")
			.css("left",(e.pageX + yOffset) + "px");
	});			
};

$(document).ready(function() {
	// add the preview class for each previewed link, so we don't need to manually add them
	$("a").each(function(){
		if (this.rel !== "") {
			$(this).addClass("screenshot");
		}
	});
	screenshotPreview();
});
