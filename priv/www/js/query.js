$('document').ready(function(){
    // Set results per page from local storage
    var results_per_page = localStorage.getItem('ets_ui_results_per_page');
    if(results_per_page != null){
        $("#pagesize option[value='"+results_per_page+"']").attr("selected", true);
    }
    var table = getUrlParameter('table');
    $('#tblname').append(table);
    do_page_query(table);
});

function do_page_query(table){
    var query_type = "page";
    localStorage.setItem('ets_ui_query_type', query_type);
    var pagesize = $('#pagesize').val();
    var query_portion =
        "table="+encodeURIComponent(table)+
        "&pagesize="+pagesize
    do_query(query_portion);
}

function lookup(){
    var query_type = "lookup";
    var table = getUrlParameter('table');
    var ets_key = $('#ets_key').val();
    if( table != '' && ets_key != '' ){
        localStorage.setItem('ets_ui_query_type', query_type);
        var query_portion =
            'table='+encodeURIComponent(table)+
            '&key='+encodeURIComponent(ets_key)+
            '&query_type='+query_type;
        do_query(query_portion);
    }
}

function match_object(){
    var query_type = "match_object";
    var table = getUrlParameter('table');
    var match_object_spec = $('#match_object_spec').val();
    var pagesize = $('#pagesize').val();
    if( match_object_spec != '' && match_object_spec != '' ){
        localStorage.setItem('ets_ui_query_type', query_type);
        var query_portion =
        'table='+encodeURIComponent(table)+
        '&tuple_wildcard='+encodeURIComponent(match_object_spec)+
        '&query_type='+query_type+
        '&pagesize='+pagesize;
        do_query(query_portion);
    }
}

function match(){
    var query_type = "match";
    var table = getUrlParameter('table');
    var match_spec = $('#match_spec').val();
    var pagesize = $('#pagesize').val();
    if( match_spec != '' && match_spec != '' ){
        localStorage.setItem('ets_ui_query_type', query_type);
        var query_portion =
        'table='+encodeURIComponent(table)+
        '&tuple_wildcard='+encodeURIComponent(match_spec)+
        '&query_type='+query_type+
        '&pagesize='+pagesize;
        do_query(query_portion);
    }
}

function next() {
    var table = getUrlParameter('table');
    var query_portion = 'table='+encodeURIComponent(table);
    var local_storage_query_type = localStorage.getItem('ets_ui_query_type');
    // Continuation
    var continuation = localStorage.getItem('ets_ui_continuation');
    console.log('continuation to use for next() '+ continuation);
    var pagesize = $('#pagesize').val();
    // Query Type
    if (local_storage_query_type == 'page' ){
        if( continuation != null ){
            query_portion +=
                '&continuation='+encodeURIComponent(continuation)+
                "&pagesize="+pagesize+
                '&query_type=page';
            do_query(query_portion);
        }
    } else if ( local_storage_query_type == 'match_object') {
        if( continuation != null ){
            query_portion +=
                '&continuation='+encodeURIComponent(continuation)+
                "&pagesize="+pagesize+
                '&query_type=match_object';
            do_query(query_portion);
        }
    }

}

function fetch(){
    var table = getUrlParameter('table');
    localStorage.removeItem('ets_ui_continuation');
    $('#ets_key').val(null);
    $('#match_object_spec').val(null);
    $('#match_spec').val(null);
    $('#ms_value').val(null);
    do_page_query(table);
}

function do_query(query_portion){
    console.log(query_portion);
    // $.get("/api/query?"+query_portion)
    //     .done(function(data){
    //         $('#error_block').empty();
    //         get_results_and_rehighlight_results(data);
    //     })
    //     .fail(function(data){
    //         $('#error_block').empty();
    //         console.log(data);
    //         console.log('backend error '+data.responseText);
    //         $('#error_block').append(data.responseText);
    //     })
    try {
        $.get("/api/query?"+query_portion, function(data, statusText, xhr){
            //alert(xhr.status);
            if(xhr.status == 200){
                $('#error_block').empty();
                get_results_and_rehighlight_results(data);
            } else {
                $('#error_block').empty();
                console.log(data);
                console.log('backend error '+data.responseText);
                $('#error_block').append(data.responseText);
            }
        })
    } catch(error) {
        console.log('.get error ' + error);
    }
}

function get_results_and_rehighlight_results(data) {
    // Show the next button as greyed out when no results
    if( data.rows.length <= 0 ){
        $('#next').attr('class', 'btn btn-secondary');
    } else {
        $('#next').attr('class', 'btn btn-primary');
    }
    console.log('received continuation ' + data.continuation);
    console.log('received '+data.rows.length+' rows ');
    if( data.continuation == undefined ){
        console.log('clear continuation');
        localStorage.removeItem('ets_ui_continuation');
        $('#next').attr('class', 'btn btn-secondary');
    } else {
        localStorage.setItem('ets_ui_continuation', data.continuation);
        $('#next').attr('class', 'btn btn-primary');
    }
    if(data.rows.length != 0){
        $('#erlang_entries').empty();
        for (var i = 0; i <= data.rows.length - 1; i++) {
            var entry = data.rows[i];
            var erlang_tuple = '{';
            for (var j = 0; j <= entry.values.length - 1; j++) {
                var cell = entry.values[j][1];
                if(j == entry.values.length - 1){
                    erlang_tuple += cell;
                }else{
                    erlang_tuple += cell+',';
                }
            }
            erlang_tuple += '}' + '<br /><hr />';
            $('#erlang_entries').append(erlang_tuple);
        }
        $('#query_results').empty();
        var jsonstring = JSON.stringify(data.rows, null, 2);
        $('#query_results').append(jsonstring);
        re_highlight_block();
    } else {
        // at least show that nothing returned...
        $('#erlang_entries').empty();
        $('#erlang_entries').append(data.rows);
    }
}

function re_highlight_block() {
    document.querySelectorAll('pre code').forEach((block) => {
        hljs.highlightBlock(block);
    });
}

$("#lookup").click(function(){
    lookup();
});

$("#match_object_btn").click(function(){
    match_object();
});

$("#match_btn").click(function(){
    match();
});

// $("match")

$("#next").click(function(){
    next();
});

$("#fetch").click(function(){
    fetch();
});

$('#pagesize').change(function(){
    console.log('set results per page to '+$( this ).val());
    localStorage.setItem('ets_ui_results_per_page', $( this ).val())
});

$('#toggle_entry_json').click(function(){
    if ( $('#query_results_div').is(":visible") == false ){
        $('#query_results_div').show();
        $('#erlang_entries_div').hide();
    } else if( $('#query_results_div').is(":visible") ){
        $('#query_results_div').hide();
        $('#erlang_entries_div').show();
    }
});

$('#lookup_erl_code').click(function(){
    var table = getUrlParameter('table');
    var ets_key = $('#ets_key').val();



    if( table != '' && ets_key != '' ){

        // // fix key to be code ready
        // var first_char = ets_key.substring(0, 1);
        // if( first_char non-alpha [a-z] or first_char == '\'' ){
        //     ets_key = '\''+ets_key+'\'';
        // } else {

        // }

        alert('ets:lookup('+table+', '+ets_key+')');
    } else {
        alert('Enter a key!');
        $('#ets_key').focus();
    }
});
