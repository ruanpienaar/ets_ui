import React from "react";
import Header from './Header';
import EtsTables from "./EtsTables";
import EtsTableView from './EtsTableView.js';
import $ from "jquery";

// TODO: make the table-view / entry-view boolean flags, into function to simplify & prevent bugs.
// TODO: create init state function. state management is getting messy

export default class EtsUiApp extends React.Component {
    state = {
        viewSystemTables: false,
        liveView: false, // allow for live-view(ws), or polling(GET calls)
        tables: [],
        selectedTable: undefined,
        pagesize_options: [5, 10, 20, 50],
        pagesize: undefined,
        tableRows: [],
        showTables: undefined,
        showTableRows: undefined,
        continuation: undefined,
        resetQuery: undefined
    }
    setStateTables = (tables) => {
        //const table_names = tables.map((t) => (t.name));
        if(tables !== this.state.tables) {
            //console.log(' tables has changed sine last time '+this.state.tables);
            //console.log(' the new tables are '+tables);
            //let pagesize = this.state.pagesize_options[0];
            const lsPagesize = localStorage.getItem('pagesize');
            let pagesize = this.state.pagesize_options[0];
            if(lsPagesize){
                pagesize = lsPagesize;
            }
            this.setState((prevState) => ({
                selectedTable: undefined,
                showTables: true,
                showTableRows: false,
                tableRows: undefined,
                tables: tables,
                pagesize: pagesize,
                continuation: undefined,
                resetQuery: undefined
            }));
        } else {
            //console.log('state did not change...');
        }
    }
    fetchTables = () => {
        //console.log('fetch tables viewSystemTables='+this.state.viewSystemTables);
        const ss = this.setStateTables;
        $.get("/api/tables", { viewSystemTables: this.state.viewSystemTables })
        .done(function( data ) {
            ss(data.tables);
        });
    }
    toggleViewSystemTables = () => {
        //console.log('toggleViewSystemTables '+this.state.viewSystemTables);
        this.setState((prevState) => ({ viewSystemTables: !prevState.viewSystemTables }) );
    }
    // TODO: cleanup setstate, why in a sep. function? In a seperate function because of the jQuery .done function.
    setStateRows = (selectedTable, data) => {
        this.setState(() => ({
            selectedTable: selectedTable,
            showTables: false,
            showTableRows: true,
            tableRows: data.rows,
            continuation: data.continuation,
            resetQuery: undefined
        }));
    }
    // TODO: do we need encodeURIComponent( in URI components ?
    queryTable = (selectedTable) => {
        if(!selectedTable){
            selectedTable=this.state.selectedTable;
        }
        console.log('queryTable table='+selectedTable+' pagesize='+ this.state.pagesize+' continuation='+this.state.continuation);
        const sr = this.setStateRows;
        let queryArgs = {
            table: selectedTable,
            pagesize: this.state.pagesize
        };
        if(this.state.continuation){
            queryArgs.continuation = this.state.continuation;
        }
        console.log('query arguments ');
        Object.keys(queryArgs).forEach(prop => console.log('prop='+prop+'value='+queryArgs[prop]));
        $.get("/api/query", queryArgs)
        .done(function(data) {
            sr(selectedTable, data)
        });
    }
    logState = () => {
        console.log('state.viewSystemTables = '+this.state.viewSystemTables);
        console.log('state.liveView = '+this.state.liveView);
        console.log('state.tables = '+this.state.tables);
        console.log('state.selectedTable = '+this.state.selectedTable);
        console.log('state.pagesize_options = '+this.state.pagesize_options);
        console.log('state.pagesize = '+this.state.pagesize);
        console.log('state.tableRows = '+this.state.tableRows);
        console.log('state.showTables = '+this.state.showTables);
        console.log('state.showTableRows = '+this.state.showTableRows);
        console.log('state.continuation = '+this.state.continuation);
    }
    nextEntries = () => {
        console.log('fetch data for table '+this.state.selectedTable+' pagesize '+ this.state.pagesize);
        this.queryTable();
    }
    changePagesize = (selectedObject) => {
        //Object.keys(selectedObject).forEach(prop => console.log(prop));
        //console.log('changePagesize='+selectedObject.target.value);
        const pagesize = selectedObject.target.value;
        localStorage.setItem('pagesize', pagesize);
        this.setState(() => ({
            pagesize: pagesize,
            continuation: undefined,
            resetQuery: true
        }));
    }
    tableView = () => (
            <EtsTables
                tables={this.state.tables}
                queryTable={this.queryTable}
            />
    );
    entriesView = () => (
        <EtsTableView
            rows={this.state.tableRows}
            nextEntries={this.nextEntries}
            continuation={this.state.continuation}
            pagesize={this.state.pagesize}
            pagesize_options={this.state.pagesize_options}
            changePagesize={this.changePagesize}
        />
    );
    componentDidMount(){
        // NB! dumb assumption that hosting on HTTPS,
        //     will also have WSS calls to backend.
        // const proto = window.protocol;
        // let ws_proto;
        // if ( proto == "http:") {
        //     ws_proto = "ws:";
        // } else if ( proto == "https:" ) {
        //     ws_proto = "wss:";
        // } else {
        //     ws_proto = "ws:";
        // }
        //var ws = new WebSocket(ws_proto + window.hostname + "/ws");
        this.fetchTables();
    }
    componentDidUpdate(prevProps, prevState){
        if( prevState.viewSystemTables != this.state.viewSystemTables ) {
            // view system tables state changed (button press), fetch tables again...
            this.fetchTables();
        }
        // here we assume, that the user is querying the table
        // console.log('componentDidUpdate this.state.continuation = '+this.state.continuation);
        // console.log('componentDidUpdate this.state.resetQuery = '+this.state.resetQuery);
        if( this.state.resetQuery ) {
            this.nextEntries();
        }
    }
    render(){
        return (
            <div>
                <Header
                    logState={this.logState}
                    toggleViewSystemTables={this.toggleViewSystemTables}
                    viewSystemTables={this.state.viewSystemTables}
                    fetchTables={this.fetchTables}
                />
                { this.state.showTables && this.tableView() }
                { this.state.showTableRows && this.entriesView() }
            </div>
        );
    }
}