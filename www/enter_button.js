$(document).keyup(function(event) {
    if (($("#query").is(":focus") && (event.key == "Enter")) || 
    ($("#num_choices").is(":focus") && (event.key == "Enter"))){
        $("#button").click();
    }
});