$('document').ready(function(){
    // "/api/table/:table_name"
    $.get("/api/table/all/"+getUrlParameter('table'), function( data ){
        console.log(data);
        $('#all').append(data);
    })
});