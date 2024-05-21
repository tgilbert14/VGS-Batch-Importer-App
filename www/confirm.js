$(document).ready(function() {
  // Mode Button
  $("#mode").on("change", async function(event) {
    // Check the state of the checkbox and change the title accordingly
    var title = $("#mode").is(":checked") ? "QA/QC mode engaged" : "QA/QC mode dis-engaged";
    
    Swal.fire({
      title: title,
      focusConfirm: false,
      confirmButtonText: `meow`,
      background: "orange",
      backdrop: `
        rgba(0,0,123,0.4)
        url("/images/nyan-cat.gif")
        left top
        no-repeat
      `
    });
  });
  
  // qaqc file button
    $("#qaqc").on("change", async function(event) {
    // Check the state of the checkbox and change the title accordingly
    var title = $("#qaqc").is(":checked") ? "Using 'SpeciesReplace.xlsx' to update species!" : "Never mind...";
    
    Swal.fire({
      title: title,
      focusConfirm: false,
      confirmButtonText: `meow`,
      background: "orange",
      backdrop: `
        rgba(0,0,40,10)
        url("/images/nyan-cat.gif")
        left top
        no-repeat
      `
    });
  });
  
    // Special data insert Button
  $("#spec_mode").on("change", async function(event) {
    // Check the state of the checkbox and change the title accordingly
    var title = $("#spec_mode").is(":checked") ? "Special Insert - only use specific files with 'incomplete transects' in this mode" : "nevermind...";
    
    Swal.fire({
      title: title,
      focusConfirm: false,
      confirmButtonText: `meow`,
      background: "purple",
      backdrop: `
        rgb(115, 63, 213)
        url("/images/nyan-cat.gif")
        left top
        no-repeat
      `
    });
  });
  
});
