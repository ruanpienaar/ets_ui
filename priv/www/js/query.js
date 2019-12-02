$('document').ready(function(){
    //alert(getUrlParameter('table'));
    var table = getUrlParameter('table');
    $('#tblname').append(table);
    // var page_load_ets_key = getUrlParameter('key');
    // if( page_load_ets_key != ''){
    //     $('#ets_key').val(page_load_ets_key);
    // }
    // var page_load_ets_value = getUrlParameter('value');
    // if( page_load_ets_value != ''){
    //     $('#ets_value').val(page_load_ets_value);
    // }

    // var query_type = getUrlParameter('query_type');
    // if ( query_type != ''){
    //     //alert(query_type);
    //     // lookup
    //     if (query_type == 'lookup') {
    //         $('#query_type').prop('checked', true);
    //     } if (query_type == 'match_object'){
    //         $('#query_type').prop('checked', false);
    //     }
    //     // match_object
    // } else {
    //     //query_type = "checked"
    //     $('#query_type').prop('checked', true);
    // }

    // if( page_load_ets_key != '' || page_load_ets_value != '' ){

    //     var url = window.location.href;
    //     var Qstring = url.split("?")[1];
    //      alert(arr[1]);

    //     alert("/api/query?"+Qstring);

    //     // Do the query here as AJAX call
    //     $.get("/api/query?"+Qstring, function( data ){
    //         $('#query_results').empty();
    //         $('#query_results').append(data);
    //     });
    // }




    // take the current URL and query...


    // so on arrival, only table willl be set,
    // so fetch the first X entries, and return the results + conti_key
    do_page_query(table);

});

function do_page_query(table){
    console.log(table);
    console.log("/api/query?table="+table);
    $.get("/api/query?table="+encodeURIComponent(table), function( data ){
        $('#query_results').empty();
        var jsonstring = JSON.stringify(data, null, 2);
        $('#query_results').append(jsonstring);
        hljs.initHighlighting();
    });
}

function get_results(){
    var table = getUrlParameter('table');
    var ets_key = $('#ets_key').val();
    var key_type = $('#key_type').val();
    if( ets_key != '' ){
        var query_portion =
            'table='+encodeURIComponent(table)+
            '&key='+encodeURIComponent(ets_key)+
            '&key_type='+encodeURIComponent(key_type);
        $.get("/api/query?"+query_portion, function( data ){
            $('#query_results').empty();
            var jsonstring = JSON.stringify(data, null, 2);
            $('#query_results').append(jsonstring);
            hljs.initHighlighting();
        });
    }
}

function tuple_wildcard(){
    var table = getUrlParameter('table');
    var tuple_value = $('#tuple_value').val();
    if( tuple_value != '' ){
        var query_portion =
        'table='+encodeURIComponent(table)+
        '&tuple_wildcard='+encodeURIComponent(tuple_value)
        $.get("/api/query?"+query_portion, function( data ){
            $('#query_results').empty();
            var jsonstring = JSON.stringify(data, null, 2);
            $('#query_results').append(jsonstring);
            hljs.initHighlighting();
        });
    }
}

$("#lookup").click(function(){
    get_results();
});

$("#tuple_wildcard").click(function(){
    tuple_wildcard();
});


$("#page").click(function(){
    window.location.href = 'page.html?table='+getUrlParameter('table');
});
$("#download").click(function(){
    alert('not implemented yet');
});
