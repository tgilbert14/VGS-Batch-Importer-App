$(document).ready(function() {
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
});
