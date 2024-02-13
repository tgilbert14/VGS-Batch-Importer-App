$(document).ready(function() {
  var upload = false;
  $("#mode").on("click", async function(event) {
/*    if(!create) {
      create = true;
      return true;
    }
    event.preventDefault();
    const { value: yes } = await Swal.fire({
      title: "Really upload a new file?",
      text: "If you do so, the current state of the app will be lost.",
      showDenyButton: true,
      confirmButtonText: "Yes",
      denyButtonText: "No"
    });
    if(yes) {
      create = false;
      $("#create").click();
    }*/
    Swal.fire({
  title:"Choose batch files -cat",
  //width: 500,
  //padding: "4em",
  //color: "dark gray",
  //background: "#fff url(/images/cowboy3_small.png)",
  showCancelButton: true,
  focusConfirm: false,
    confirmButtonText: `Thanks Cat!`,
    cancelButtonText: `No thanks`,
  background: "orange",
  backdrop: `
    rgba(0,0,123,0.4)
    url("/images/nyan-cat.gif")
    left top
    no-repeat
  `
});
  });
  
    $("#mode").on("click", async function(event) {
    Swal.fire({
  title:"QA/QC mode engaged",
  //width: 500,
  //padding: "4em",
  //color: "dark gray",
  //background: "#fff url(/images/cowboy3_small.png)",
  //showCancelButton: true,
  focusConfirm: false,
    confirmButtonText: `meow`,
    //cancelButtonText: `No thanks`,
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