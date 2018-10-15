
// Function forcing plotly plots to scale to div/window width
function autorangeChart(div) {
    Plotly.relayout(div, {
        'xaxis.autorange': true,
        'yaxis.autorange': true
    });
}

/* This function creates a new page, copies relevant content to
   it, opens the print dialogue and closes the page after printing. */
   
function printDiv(event) {
  var divName = $(this).closest(".col-sm-12");
  //var svgDiv = document.getElementById("materials-checkouts_plot_all");
  var widgetDivs = document.getElementsByClassName("html-widget-output");
  [].forEach.call(widgetDivs, function (widgetDiv) {
      widgetDiv.setAttribute('style','width:700px;');
      console.log(widgetDiv.id);
      //autorangeChart(widgetDiv);
  });
  /* If width is above 700px then set it to that, else keep current width? */
  //svgDiv.setAttribute('style','width:700px;');
  //html-widget-output
  //autorangeChart('materials-checkouts_plot_all');
  //var printContents = document.getElementById(divName).innerHTML;
  w = window.open();  // open a new window
  //w.document.write(printContents);
  w.document.write($(divName).html());
  var svgs = w.document.getElementsByClassName("main-svg");  // get refs to all elements with class main-svg
  [].forEach.call(svgs, function (svg) {svg.setAttribute('style','position:absolute;')});  // set position absolute for all svgs to make axis label svg and graph svg stay on top of each other
  //var inputs = w.document.getElementsByTagName("input"); 
  //[].forEach.call(inputs, function (input) {input.setAttribute('style','display:none;')}); 
  var inputs = w.document.getElementsByTagName("input");  // get refs to all input elements (e.g. the print button)
  while (inputs[0]) inputs[0].parentNode.removeChild(inputs[0]);  // remove all input elements
  var modebars = w.document.getElementsByClassName("modebar");
  while (modebars[0]) modebars[0].parentNode.removeChild(modebars[0]);  // remove all modebar elements
  w.print(); // open the print dialog
  w.close(); // close the new window
  svgDiv.setAttribute('style','width:100%;'); // reset plot width to original (100%)
  autorangeChart('materials-checkouts_plot_all'); // force relayout on plot again to make it fit the width of it's div
}

/*
TODO: særlig function kun tilgængelig for sekretariatet (via proxyens env var med login_navn) 
som viser en knap ved siden af printknappen til at hive svg'er ud i bestemte bredder til den fysiske 
whitebook og som overholder de ændringer, der er lavet med filtre!

  var divName = $(this).closest(".col-sm-12");
  $(divName).children(".html-widget-output").css('width','700px');
  $(divName).children(".html-widget-output").each(function() {
      console.log(this.id);
      autorangeChart(this.id);
    }
  );

  //var svg = svgs[0];
  //var svgb = svgs[1];
  //svg.setAttribute('style','position:absolute;');
  //svgb.setAttribute('style','position:absolute;');

//maybe call this in a 'on window resize' event

  Plotly.relayout('materials-checkouts_plot_all', {
    'xaxis.autorange': true,
    'yaxis.autorange': true
  });

  //var sc = "<link rel='stylesheet' href='plotprint.css' type='text/css'>";

  var sc = w.document.createElement("script");
  sc.setAttribute("src", "htmlwidgets-1.2/htmlwidgets.js");
  w.document.head.appendChild(sc);
  var sc = w.document.createElement("script");
  sc.setAttribute("src", "plotly-binding-4.8.0/plotly.js");
  w.document.head.appendChild(sc);

  //var sc = w.document.createElement("script");
  //sc.setAttribute("rel", "stylesheet");
  //sc.setAttribute("href", "plotprint.css");
  //sc.setAttribute("type", "text/css");
  //w.document.head.appendChild(sc);


  //svg.setAttribute('viewbox','800 100 600 600');
  //svgb.setAttribute('viewbox','800 100 600 600');

function printDiv(divName) {
  var svg = $('.main-svg')[0];
  //var svg = $('.plotteren')[0];
  //svg.setAttribute("style","width:600px");
  //svg.setAttribute("width","600px");
  svg.setAttribute('viewbox', '800 0 600 600');
  var printContents = document.getElementById(divName).innerHTML;
  w=window.open();
  //$(':button').addClass('hidden');
  w.document.write(printContents);
  var linkElement = "<link rel='stylesheet' href='/css/masterBlaster.css' type='text/css' media='screen'>";
  $(document.documentElement.head).append(linkElement);
  w.print();
  //w.close();
  $(':button').removeClass('hidden');
}

function printDiv(printClass) {
  //$('body').width( '600px' );
  var svg = $('.main-svg')[0];
  //var svg = $('.plotteren')[0];
  //svg.setAttribute("style","width:600px");
  //svg.setAttribute("width","600px");
  //Plotly.relayout(svg);
  //var divName = $(this).closest(printClass);
  var divName = $(this).closest(".col-sm-12");
  console.log(this);
  svg.setAttribute('viewbox', '800 100 600 600');
  //var printContents = document.getElementById(divName).innerHTML;
  var printContents = divName.innerHTML;
  w = window.open();
  w.document.write(printContents);
  w.print();
  w.close();
}
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