$('document').ready(function(){
    const data = []
    // get all nodes
    $.get("/api/nodes", function( data ){
        var nodes = data.nodes;
        for (var i = nodes.length - 1; i >= 0; i--) {
            data.push({nodes[i].host})
        }
    },
    $("#example").DropDownTree({
        data: data,
        // dropdown title
        title : "Nodes",
        // close/open icons
        closedArrow : '<i class="fa fa-caret-right" aria-hidden="true"></i>',
        openedArrow : '<i class="fa fa-caret-down" aria-hidden="true"></i>',
        // max height of dropdown
        maxHeight : 300,
        // is multiple select?
        multiSelect : true,
        // checks all children of the selected parent
        selectChildren : false,
        // if the children are dynamic
        addChildren: false,
        // if is RTL(RIGHT TO LEFT)?
        rtl: false
    });
});


// const data = [
//       {
//         title:"HTML5",href:"#1",
//         dataAttrs:[
//             {title:"dataattr1",data:"value1"},
//             {title:"dataattr2",data:"value2"},
//             {title:"dataattr3",data:"value3"}
//         ]
//       }
// ];

// var subData = [
//     {
//       title:"jQueryScript",
//       href:"#1",
//       dataAttrs:[{title:"dataattr1",data:"value1"},{title:"dataattr2",data:"value2"},{title:"dataattr3",data:"value3"}]
//     }
// ];

// $("#example").DropDownTree({
//   data: data
// });

// $("#example").DropDownTree({
//   data: data,
//   // dropdown title
//   title : "Nodes",
//   // close/open icons
//   closedArrow : '<i class="fa fa-caret-right" aria-hidden="true"></i>',
//   openedArrow : '<i class="fa fa-caret-down" aria-hidden="true"></i>',
//   // max height of dropdown
//   maxHeight : 300,
//   // is multiple select?
//   multiSelect : true,
//   // checks all children of the selected parent
//   selectChildren : false,
//   // if the children are dynamic
//   addChildren: false,
//   // if is RTL(RIGHT TO LEFT)?
//   rtl: false
// });


// $("#example").DropDownTree({

//   clickHandler : function(target){
//     alert(target);
//   },
//   expandHandler : function(target,expanded){
//     alert(target);
//   },
//   checkHandler : function(target,checked){
//     alert(target);
//   },

// });
