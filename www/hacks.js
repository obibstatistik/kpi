
// Function forcing plotly plots to scale to div/window width

function autorangeChart(div) {
    
    // if svg has a slice, we assume it's a pie...
    if($('#' + div).find('.slice').length !== 0) {
      Plotly.relayout(div, {
          'height.autorange': true, 
          'width.autorange': true
      });
    } else {
      Plotly.relayout(div, {
          'xaxis.autorange': true,
          'yaxis.autorange': true
      });
    }
}

/* This function creates a new page, copies relevant content to
   it, opens the print dialogue and closes the page after printing. */

function printDiv(event, parentClass, divWidth, divHeight) {
  var isFirefox = typeof InstallTrigger !== 'undefined';                                           // semireliable check for firefox (by looking for it's apparently unique installtrigger feature)
  var divName = $(this).closest(parentClass);                                                      // choose a parent div (based on it's class) to copy to a new window/html document for printing
  var widgetDivs = $(divName).find(".html-widget-output.plotly");                                  // find() all divs among the descendants of the print div with classes .html-widget-output and .plotly
  widgetDivs.css('width', divWidth);                                                               // set the width of the svg elements to the one from the method's parameters
 
  if (divHeight) {
    widgetDivs.css('height', divHeight);                                                           // if height is set in parameters, then apply it
  }
  widgetDivs.each(function() { autorangeChart(this.id); });                                     

  w = window.open();                                                                               // open a new window/html document
  w.document.write('<link href="plotprint.css" type="text/css", rel="stylesheet" />' + $(divName).html());       // write css link and the saved html to the new, empty window
  
  var svgs = w.document.getElementsByClassName("main-svg");                                        // get refs to all elements with class main-svg, this time in the new window
  [].forEach.call(svgs, function (svg) {
    svg.setAttribute('style','position:absolute;');
  });                                                                                              // set position absolute for all svgs to make axis label svg and graph svg stay on top of each other
  
  var ylines = w.document.getElementsByClassName("ygrid");
  [].forEach.call(ylines, function (yline) {  
    yline.setAttribute('style','stroke:black; stroke-width: 0.5px;');                              // set width of horizontal gridlines to something more visible in print.
  });
  
  var sc = w.document.createElement("link");                                                       // create a link element to put in the new document to point to a css file for styling the print page
  sc.setAttribute("rel", "stylesheet");                                                            // add the necessary attributes to the link
  sc.setAttribute("type", "text/css");
  sc.setAttribute("href", "plotprint.css");
  w.document.head.appendChild(sc);                                                                 // append the element to the body of the new document

  if(isFirefox) {                                                                                  // firefox does not like waiting for onload for some reason
    w.print(); 
    w.close(); 
  } else {
    sc.onload = function(){ w.print(); w.close(); };                                               // wait for the link element to be loaded before calling print() function and then close() after that
  }

  if (divHeight) {
    widgetDivs.css('height', '100%');                                                              // if height is set in parameters, then reset it to 100%. TODO: read the px height beforehand and use it here
  }
  widgetDivs.css('width','100%');                                                                  // reset widths of plot divs and the like
  widgetDivs.each(function() { autorangeChart(this.id); });                                        
}

/*
// Force rescaling of Plotly plots when browser window changes size
window.addEventListener("resize", function(event) {
  console.log("resizing!");
  $(document).find(".html-widget-output.plotly").each(function() { console.log(this.id); autorangeChart(this.id); });
});
*/

document.addEventListener("DOMContentLoaded", function(event) {

  //arrangementer
  $( "li.active i.fa-calendar" ).replaceWith( "<img class=\"active\" src=\"icons/arrangementer_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-calendar" ).replaceWith( "<img class=\"1\" src=\"icons/arrangementer_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".1" ).parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/arrangementer_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/arrangementer_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
  
  //fysiskerum
  $( "li.active i.fa-building").replaceWith( "<img class=\"active\" src=\"icons/detfysiskerum_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-building" ).replaceWith( "<img class=\"2\" src=\"icons/detfysiskerum_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".2").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/detfysiskerum_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/detfysiskerum_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
  
  //online
  $( "li.active i.fa-laptop").replaceWith( "<img class=\"active\" src=\"icons/online_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-laptop" ).replaceWith( "<img class=\"3\" src=\"icons/online_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".3").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/online_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/online_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
  
  //materialer
  $( "li.active i.fa-book").replaceWith( "<img class=\"active\" src=\"icons/materialer_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-book" ).replaceWith( "<img class=\"4\" src=\"icons/materialer_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".4").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/materialer_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/materialer_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
  
  //eressourcer
  $( "li.active i.fa-database").replaceWith( "<img class=\"active\" src=\"icons/eressourcer_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-database" ).first().replaceWith( "<img class=\"5\" src=\"icons/eressourcer_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".5").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/eressourcer_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/eressourcer_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
  
  //brugere
  $( "li.active i.fa-users").replaceWith( "<img class=\"active\" src=\"icons/brugere_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-users" ).first().replaceWith( "<img class=\"6\" src=\"icons/brugere_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".6").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/brugere_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/brugere_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
  
  //databases
  $( "li.active i.fa-database").replaceWith( "<img class=\"active\" src=\"icons/datakilder_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-database" ).replaceWith( "<img class=\"7\" src=\"icons/datakilder_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".7").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/datakilder_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/datakilder_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
});