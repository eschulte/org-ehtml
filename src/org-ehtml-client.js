// Copyright (C) 2012 Eric Schulte <eric.schulte@gmx.com>
// License GPLV3

// after the page loads, run the set set_clickable function
$(document).ready( function(){ set_clickable(); } );

function set_clickable(){
  // to every element with class="edit_in_place"
  $('.edit_in_place').click(function(){
    var org  = $(this).next().html();
    var beg  = $(this).next().attr("contents-begin");
    var end  = $(this).next().attr("contents-end");
    var html = $(this).html();
    $(this).after(
      '<div><textarea rows="10" cols="80">'+org+'</textarea><div>'+
        '<input type="button" value="SAVE" class="save_button" />'+
        '<input type="button" value="CANCEL" class="cancel_button" />'+
        '</div></div>');
    // remove the orignal html
    $(this).remove();
    // call these functions when buttons are hit
    $('.save_button').click(function(){
      save_changes(this, $(this).parent().prev().val(), beg, end);
    });
    $('.cancel_button').click(function(){ abort_changes(this, html); });
  });
  // make every div with class="edit_in_place" highlight on mouseover
  $('.edit_in_place').mouseover(function(){ $(this).addClass("editable"); });
  $('.edit_in_place').mouseout(function() { $(this).removeClass("editable"); });
};

function save_changes(obj, org, beg, end){
  var here = window.location.pathname;
  $.ajax({
    type: 'POST',
    url: here,
    data: {org:  org,
           beg:  beg,
           end:  end,
           path: here},
    statusCode: {
      200: function(html,status){
        $(obj).parent().parent().next().after(
          '<div class="edit_in_place">'+html+'</div>'+
            '<div class="raw-org" contents-begin="'+beg+
            '" contents-end="'+(org.length+Number(beg))+'">'+org+'</div>');
        $(obj).parent().parent().next().remove();
        $(obj).parent().parent().remove();
        set_clickable();
      },
      401: function(){ window.location.replace("/login/?to="+here); },
      500: function(error){ alert('error:'+error); }
    }
  });
};

function abort_changes(obj, old_html){
  $(obj).parent().parent().after(
    '<div class="edit_in_place">'+old_html+'</div>');
  $(obj).parent().parent().remove();
  set_clickable();
};
