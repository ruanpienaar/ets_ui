import React from "react";
import Header from './Header';
import EtsTables from "./EtsTables";
import EtsTableView from './EtsTableView.js';
import $ from "jquery";

// TODO: make the table-view / entry-view boolean flags, into function to simplify & prevent bugs.

export default class EtsUiApp extends React.Component {
    state = {
        viewSystemTables: false,
        liveView: false, // allow for live-view(ws), or polling(GET calls)
        tables: [],
        selectedTable: undefined,
        pagesize: 3,
        tableRows: [],
        showTables: undefined,
        showTableRows: undefined,
        continuation: undefined
    }
    setStateTables = (tables) => {
        //const table_names = tables.map((t) => (t.name));
        if(tables !== this.state.tables) {
            //console.log(' tables has changed sine last time '+this.state.tables);
            //console.log(' the new tables are '+tables);
            this.setState((prevState) => ({
                showTables: true,
                showTableRows: false,
                tables: tables
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
            continuation: data.continuation
        }));
    }
    // TODO: do we need encodeURIComponent( in URI components ?
    queryTable = (selectedTable) => {
        console.log('fetch data for table '+selectedTable+' pagesize '+ this.state.pagesize);
        const sr = this.setStateRows;
        let queryArgs = {
            table: selectedTable,
            pagesize: this.state.pagesize
        };
        if(this.state.continuation){
            queryArgs.continuation = this.state.continuation;
        }
        console.log(queryArgs);
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
        console.log('state.pagesize = '+this.state.pagesize);
        console.log('state.tableRows = '+this.state.tableRows);
        console.log('state.showTables = '+this.state.showTables);
        console.log('state.showTableRows = '+this.state.showTableRows);
        console.log('state.continuation = '+this.state.continuation);
    }
    nextEntries = () => {
        console.log('fetch data for table '+this.state.selectedTable+' pagesize '+ this.state.pagesize);
        this.queryTable(this.state.selectedTable);
    }
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
        //console.log('state.viewSystemTables '+this.state.viewSystemTables);
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
                { this.state.showTables && <EtsTables tables={this.state.tables} queryTable={this.queryTable} /> }
                { this.state.showTableRows && <EtsTableView rows={this.state.tableRows} nextEntries={this.nextEntries} continuation={this.state.continuation} /> }
            </div>
        );
    }
}