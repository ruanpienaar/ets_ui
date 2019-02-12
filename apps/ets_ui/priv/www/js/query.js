$('document').ready(function(){
    //alert(getUrlParameter('table'));
});

$("#search").click(function(){
    //
});
$("#all").click(function(){
    window.location.href = 'all.html?table='+getUrlParameter('table');
});
$("#download").click(function(){
    alert('not implemented yet');
});

