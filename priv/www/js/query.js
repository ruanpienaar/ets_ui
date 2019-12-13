$('document').ready(function(){
    //alert(getUrlParameter('table'));
    var table = getUrlParameter('table');
    $('#tblname').append(table);
    // so on arrival, only table willl be set,
    // so fetch the first X entries, and return the results + conti_key
    do_page_query(table);
});

function do_page_query(table){
    do_query("table="+table, hljs);
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
        do_query(query_portion, hljs);
    }
}

function tuple_wildcard(){
    var table = getUrlParameter('table');
    var tuple_value = $('#tuple_value').val();
    if( tuple_value != '' ){
        var query_portion =
        'table='+encodeURIComponent(table)+
        '&tuple_wildcard='+encodeURIComponent(tuple_value)
        do_query(query_portion, hljs);
    }
}

function get_next() {
    var table = getUrlParameter('table');
    var continuation_val = $('#continuation_val').val();
    var key_data_type_val = $('#key_data_type_val').val();
    if( continuation_val != '' && key_data_type_val != '' ){
        var query_portion =
        'table='+encodeURIComponent(table)+
        '&continuation='+encodeURIComponent(continuation_val)+
        '&key_type='+encodeURIComponent(key_data_type_val);
        do_query(query_portion, hljs);
    }
}

function do_query(query_portion, hljs){
    console.log(query_portion);
    $.get("/api/query?"+query_portion, function( data ){
        get_results_and_rehighlight_results(data, hljs);
    });
}

function get_results_and_rehighlight_results(data, hljs) {
    $('#query_results').empty();
    $('#continuation').empty();
    $('#continuation_val').empty();
    $('#key_data_type').empty();
    $('#key_data_type_val').empty();
    var jsonstring = JSON.stringify(data.rows, null, 2);
    $('#query_results').append(jsonstring);
    $('#continuation').append(data.continuation);
    $('#continuation_val').val(data.continuation);
    $('#key_data_type').append(data.key_type);
    $('#key_data_type_val').val(data.key_type);
    hljs.initHighlighting();
}

$("#lookup").click(function(){
    get_results();
});

$("#tuple_wildcard").click(function(){
    tuple_wildcard();
});

$("#next").click(function(){
    get_next();
});

// $("#page").click(function(){
//     window.location.href = 'page.html?table='+getUrlParameter('table');
// });
$("#download").click(function(){
    alert('not implemented yet');
});





