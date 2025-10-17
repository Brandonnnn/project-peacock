//$(document).ready(function() {
$(document).on("shiny:sessioninitialized", function(event) {
  /**************************** OPTION 2 ****************************/
    // Helper function to guarantee cross-browser compatibility
  // adapted from: http://stackoverflow.com/a/16157942
  function localeString(x, sep, grp) {
    var sx = (''+x).split('.'), s = '', i, j;
    sep || (sep = ',');            // default separator
    grp || grp === 0 || (grp = 3); // default grouping
    i = sx[0].length;
    while (i > grp) {
      j = i - grp;
      s = sep + sx[0].slice(j, i) + s;
      i = j;
    }
    s = sx[0].slice(0, i) + s;
    sx[0] = s;
    return sx.join('.');
  }
  
  // To change the input field (lose arrows and other functionality)
  $('#cameoPage-cameo_num_attr')[0].type = 'text';
  
  
  // To format the number when the app starts up
  $('#cameoPage-cameo_num_attr').val(localeString($('#cameoPage-cameo_num_attr').val()));
  
  // To format the number whenever the input changes
  $('#cameoPage-cameo_num_attr').keyup(function(event) {
    $(this).val(localeString($(this).val().replace(/,/g, '')));
  });
});