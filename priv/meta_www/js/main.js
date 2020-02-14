$('document').ready(function(){
    // get all nodes
    $.get("/api/nodes", function( data ){
        $('#nodes').empty();
        var nodes = data.nodes;
        for (var i = nodes.length - 1; i >= 0; i--) {
            //console.log(nodes[i].name);
            var row_str = "";
            row_str += '<tr>';
                row_str += '<td><a href="node.html?node='
                            +encodeURIComponent(nodes[i].host)+'&port='
                            +encodeURIComponent(nodes[i].port)+'">'
                        + nodes[i].host
                        + '</a></td>';
                row_str += '<td>' + nodes[i].port + '</td>';
                row_str += '<td>' + nodes[i].ping + '</td>';
            row_str += '<tr>';
            $('#nodes').append(row_str)
        }
    });
});
