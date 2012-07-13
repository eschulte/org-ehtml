// Copyright (C) 2012 Eric Schulte <eric.schulte@gmx.com>
// License GPLV3

// after the page loads, run the set set_clickable function
$(document).ready(function(){ set_clickable(); });

function set_clickable(){
  // to every element with class="edit_in_place"
  $('.edit_in_place').click(function(){
    var org = $(this).next().html();
    var html = $(this).html();
    $(this).after('<div><textarea rows="10" cols="80">'+org+'</textarea>'+
                  '<div><input type="button" value="SAVE" class="save_button" />'+
                  '     <input type="button" value="CANCEL" class="cancel_button" />'+
                  '</div></div>');
    // remove the orignal html
    $(this).remove();;
    // call these functions when buttons are hit
    $('.save_button').click(function(){ save_changes(this, html); });
    $('.cancel_button').click(function(){ abort_changes(this, html); });
  });
  // make every div with class="edit_in_place" highlight on mouseover
  $('.edit_in_place').mouseover(function(){ $(this).addClass("editable"); });
  $('.edit_in_place').mouseout(function() { $(this).removeClass("editable"); });
};

function save_changes(obj, new_html){
  $.post("update-this-org-file", {content: new_html}, function(txt){ alert(txt); });
  // In real life the above post request would hit the elnode server,
  // which would in turn respond with the HTML that we would use to
  // replace the element.  For now we'll just replace with the raw
  // text verbatim.
  $(obj).parent().parent().after('<div class="edit_in_place">'+new_html+'</div>');
  $(obj).parent().parent().remove();
  set_clickable();
};

function abort_changes(obj, old_html){
  $(obj).parent().parent().after('<div class="edit_in_place">'+old_html+'</div>');
  $(obj).parent().parent().remove();
  set_clickable();
};
