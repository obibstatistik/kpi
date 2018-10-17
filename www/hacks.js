
// Function forcing plotly plots to scale to div/window width
function autorangeChart(div) {
    Plotly.relayout(div, {
        'xaxis.autorange': true,
        'yaxis.autorange': true
    });
}

/* This function creates a new page, copies relevant content to
   it, opens the print dialogue and closes the page after printing. */
   
function printDiv(event,parentClass) {
  var divName = $(this).closest(parentClass);                                                 // choose a parent div (based on it's class) to copy to a new window/html document for printing
  var widgetDivs = $(divName).find(".html-widget-output.plotly");                             // find() all divs among the descendants of the print div with classes .html-widget-output and .plotly
  var svg = $(divName).find(".main-svg"); 
  widgetDivs.css('width','700px');
  widgetDivs.attr('style','width:700px;');
  widgetDivs.each(function() { autorangeChart(this.id); });                                   // force svgs to the parent divs' width
  w = window.open();                                                                          // open a new window/html document
  w.document.write($(divName).html());                                                        // write the saved html to the new empty window
  var svgs = w.document.getElementsByClassName("main-svg");                                   // get refs to all elements with class main-svg
  [].forEach.call(svgs, function (svg) {svg.setAttribute('style','position:absolute;')});     // set position absolute for all svgs to make axis label svg and graph svg stay on top of each other
  var a = w.document.getElementsByTagName("a");                                               // get refs to all a elements (e.g. the print button)
  while (a[0]) a[0].parentNode.removeChild(a[0]);                                             // remove all a elements
  var inputs = w.document.getElementsByTagName("input");                                      // get refs to all input elements (e.g. the print button)
  while (inputs[0]) inputs[0].parentNode.removeChild(inputs[0]);                              // remove all input elements
  var modebars = w.document.getElementsByClassName("modebar");                                // get refs to modebars i.e. the plotly toolbar
  while (modebars[0]) modebars[0].parentNode.removeChild(modebars[0]);                        // remove all modebar elements ()
  w.print();                                                                                  // open the print dialog
  w.close();                                                                                  // close the new window
  widgetDivs.css('width','100%');                                                             // reset widths of plot divs and the like
  widgetDivs.each(function() { autorangeChart(this.id); });                                   // force svgs back to original widths
}

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
  
  //
  $( "li.active i.fa-database").replaceWith( "<img class=\"active\" src=\"icons/datakilder_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( "i.fa-database" ).replaceWith( "<img class=\"6\" src=\"icons/datakilder_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
  $( ".6").parent().hover(function(e){
    $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/datakilder_negativ_15x15.png\" width=\"15px\" height=\"15px\" />");
  },
  function(e){
    var img_elem = $(this).find('img');
    if(!img_elem.parent().parent().hasClass('active')){
      $(this).find('img').replaceWith( "<img class=\"test\" src=\"icons/datakilder_positiv_15x15.png\" width=\"15px\" height=\"15px\" />");
    }
  });
});