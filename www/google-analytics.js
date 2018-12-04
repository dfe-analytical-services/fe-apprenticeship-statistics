//Google Analytics
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-116223405-2', 'auto');
  ga('send', 'pageview');

// FRONT PAGE Select dimension to see time-series of starts
  
  //Track measure of exclusion type selected
  $(document).on('change', '#breakdown', function(e) {
    ga('send', 'event', 'widget', 'front page / select dimension measure', $(e.currentTarget).val());
  }); 

// LA TAB Select LA 
  
  //Track measure of exclusion type selected
  $(document).on('change', '#select2', function(e) {
    ga('send', 'event', 'widget', 'la tab / select LA', $(e.currentTarget).val());
  }); 
  
// LA TAB Select an age group 
  
  //Track measure of exclusion type selected
  $(document).on('change', '#reason_type1', function(e) {
    ga('send', 'event', 'widget', 'la tab / select an age group', $(e.currentTarget).val());
  });   
  
// LA TAB Pick a level
  
  //Track measure of exclusion type selected
  $(document).on('change', '#reason_type2', function(e) {
    ga('send', 'event', 'widget', 'la tab / select a level', $(e.currentTarget).val());
  });  
  
  
 // MAP TAB Pick a measure
  
  //Track measure of exclusion type selected
  $(document).on('change', '#select_map', function(e) {
    ga('send', 'event', 'widget', 'map tab / select a measure', $(e.currentTarget).val());
  });   
  
  
  
  
  
  
  
  

