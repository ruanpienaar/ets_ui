import React from "react";
import Header from './Header';
import EtsTables from "./EtsTables";
import EtsTableView from './EtsTableView.js';
import $ from "jquery";

export default class EtsUiApp extends React.Component {
    state = {
        // view: 'tables', // tables / tableview / query / results
        viewSystemTables: false,
        liveView: false, // allow for live-view(ws), or polling(GET calls)
        tables: [],
        pagesize: 3,
        tableRows: [],
        showTables: undefined,
        showTableRows: undefined
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
    setStateRows = (rows) => {
        this.setState(() => ({
            showTables: false,
            showTableRows: true,
            tableRows: rows
        }));
    }
    viewTable = (table_name) => {
        console.log('fetch data for table '+table_name+' pagesize '+ this.state.pagesize);
        const sr = this.setStateRows;
        $.get("/api/query", { table: encodeURIComponent(table_name), pagesize: this.state.pagesize })
        .done(function( data ) {
            sr(data.rows);
            console.log('number of rows returned : ' + data.rows.length);
        });
    }
    logState = () => {
        console.log('state.viewSystemTables = '+this.state.viewSystemTables);
        console.log('state.liveView = '+this.state.liveView);
        console.log('state.tables = '+this.state.tables);
        console.log('state.pagesize = '+this.state.pagesize);
        console.log('state.tableRows = '+this.state.tableRows);
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
                { this.state.showTables && <EtsTables tables={this.state.tables} viewTable={this.viewTable} isViewingRows={this.state.tableRows.length} /> }
                { this.state.showTableRows && <EtsTableView rows={this.state.tableRows} /> }
            </div>
        );
    }
}