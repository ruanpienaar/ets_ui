$('document').ready(function(){

    // get all tables
    $.get("/api/tables", function( data ){
        $('#tables').empty();
        var tables = data.tables;
        for (var i = tables.length - 1; i >= 0; i--) {
            console.log(tables[i].name);
            var row_str = "";
            row_str += '<tr>';
                row_str += '<td><a href="query.html?table='+encodeURIComponent(tables[i].table)+'">' + tables[i].table    + '</a></td>';
                row_str += '<td>' + tables[i].name + '</td>';
                row_str += '<td>' + tables[i].reg_name + '</td>';
                row_str += '<td>' + tables[i].compressed + '</td>';
                row_str += '<td>' + tables[i].heir + '</td>';
                row_str += '<td>' + tables[i].id + '</td>';
                row_str += '<td>' + tables[i].keypos + '</td>';
                row_str += '<td>' + tables[i].memory + '</td>';
                row_str += '<td>' + tables[i].name + '</td>';
                row_str += '<td>' + tables[i].named_table + '</td>';
                row_str += '<td>' + tables[i].node + '</td>';
                row_str += '<td>' + tables[i].owner + '</td>';
                row_str += '<td>' + tables[i].protection + '</td>';
                row_str += '<td>' + tables[i].read_concurrency + '</td>';
                row_str += '<td>' + tables[i].size + '</td>';
                row_str += '<td>' + tables[i].type + '</td>';
                row_str += '<td>' + tables[i].write_concurrency + '</td>';
            row_str += '<tr>';
            $('#tables').append(row_str)
        }
    });

    $('th').click(function(){
        var table = $(this).parents('table').eq(0)
        var rows = table.find('tr:gt(0)').toArray().sort(comparer(  $(this).index() ) )
        this.asc = !this.asc
        if (!this.asc){rows = rows.reverse()}
        for (var i = 0; i < rows.length; i++){table.append(rows[i])}
    })
    function comparer(index) {
        return function(a, b) {
            var valA = getCellValue(a, index), valB = getCellValue(b, index)
            return $.isNumeric(valA) && $.isNumeric(valB) ? valA - valB : valA.toString().localeCompare(valB)
        }
    }
    function getCellValue(row, index){ return $(row).children('td').eq(index).text() }

});

function sort_table(){

}

// {
//     "tables": [
//         {
//             "name": "server_ssl_otp_session_cache",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64014>"
//         },
//         {
//             "name": "client_ssl_otp_session_cache",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64013>"
//         },
//         {
//             "name": "ssl_otp_crl_issuer_mapping",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64012>"
//         },
//         {
//             "name": "ssl_otp_crl_cache",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64011>"
//         },
//         {
//             "name": "ssl_otp_ca_ref_file_mapping",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64010>"
//         },
//         {
//             "name": "ssl_otp_ca_file_ref",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64009>"
//         },
//         {
//             "name": "ssl_otp_cacertificate_db",
//             "reg_name": "ssl_manager",
//             "size": 0,
//             "table": "#Ref<0.2310570460.3236560899.64008>"
//         },
//         {
//             "name": "ssl_pem_cache",
//             "reg_name": "ssl_pem_cache",
//             "size": 0,
//             "table": "ssl_pem_cache"
//         },
//         {
//             "name": "default",
//             "reg_name": "logger_std_h_default",
//             "size": 1,
//             "table": "#Ref<0.2310570460.3236560899.63970>"
//         },
//         {
//             "name": "logger",
//             "reg_name": "logger",
//             "size": 5,
//             "table": "logger"
//         },
//         {
//             "name": "cowboy_clock",
//             "reg_name": "cowboy_clock",
//             "size": 1,
//             "table": "cowboy_clock"
//         },
//         {
//             "name": "ranch_server",
//             "reg_name": "ranch_sup",
//             "size": 7,
//             "table": "ranch_server"
//         }
//     ]
// }