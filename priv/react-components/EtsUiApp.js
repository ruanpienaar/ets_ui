import React from "react";
import EtsHeader from './EtsHeader';
import EtsQuery from './EtsQuery';
import EtsTables from "./EtsTables";
import EtsTableView from './EtsTableView.js';
import $ from "jquery";

// TODO: make the table-view / entry-view boolean flags, into function to simplify & prevent bugs.
// TODO: create init state function. state management is getting messy

export default class EtsUiApp extends React.Component {
    constructor(props) {
        super(props);
        const lsPagesize = localStorage.getItem('pagesize');
        const lsViewQuery = localStorage.getItem('viewQuery');
        //console.log('EtsUiApp constructor lsViewQuery='+lsViewQuery);
        const pagesize_options = [5, 10, 20, 50];
        let pagesize = pagesize_options[0];
        if(lsPagesize){
            pagesize = JSON.parse(lsPagesize);
        }
        let viewQuery = false;
        if(lsViewQuery){
            viewQuery = JSON.parse(lsViewQuery);
        }
        //console.log('EtsUiApp.viewQuery='+viewQuery);
        //TODO: make system tables localStorage option.
        let defaultViewSystemTables = false;
        this.state = {
            viewSystemTables: defaultViewSystemTables,
            viewQuery: viewQuery,
            liveView: false, // allow for live-view(ws), or polling(GET calls)
            tables: [],
            selectedTable: undefined,
            pagesize_options: pagesize_options,
            pagesize: pagesize,
            tableRows: [],
            showTables: undefined,
            showTableRows: undefined,
            continuation: undefined,
            resetQuery: undefined,
            queryTables: [],
            prevQuery: undefined,
            prevTupleWildcard: undefined
        };
    }
    setStateTables = (tables) => {
        this.setState(() => ({
            selectedTable: undefined,
            showTables: true,
            showTableRows: false,
            tableRows: undefined,
            tables: tables,
            continuation: undefined,
            resetQuery: undefined,
            queryTables: []
        }));
    }
    //TODO: bit strange that this is the function that initialises the whole app.
    //      maybe rename it ?
    fetchTables = () => {
        //console.log('fetch tables viewSystemTables='+this.state.viewSystemTables);
        const ss = this.setStateTables;
        $.get("/api/tables", { viewSystemTables: this.state.viewSystemTables })
        .done(function( data ) {
            ss(data.tables);
        });
    }
    toggleViewSystemTables = () => {
        this.setState((prevState) => ({ viewSystemTables: !prevState.viewSystemTables }) );
    }
    toggleViewQuery = () => {
        this.setState(
            (prevState) => {
                console.log('AAAAAAAAAAAAAAAAAAAAAAAA');
                console.log('toggleViewQuery prevstate.viewQuery='+prevState.viewQuery);
                const newViewQuery = !prevState.viewQuery;
                localStorage.setItem('viewQuery', newViewQuery);
                console.log('toggleViewQuery newViewQuery='+newViewQuery);
                return { viewQuery: newViewQuery }
            }
        );
    }
    setStateRows = (selectedTable, data, queryType, prevTupleWildcard) => {
        this.setState(() => ({
            selectedTable: selectedTable,
            showTables: false,
            showTableRows: true,
            tableRows: data.rows,
            continuation: data.continuation,
            resetQuery: undefined,
            queryTables: [selectedTable],
            prevQuery: queryType,
            prevTupleWildcard: prevTupleWildcard
        }));
    }
    // TODO: do we need encodeURIComponent( in URI components ?
    queryTable = (selectedTable, queryType, key, tupleWildcard) => {

        if(!selectedTable){
            selectedTable=this.state.selectedTable;
        }
        //console.log('queryTable table='+selectedTable+' pagesize='+ this.state.pagesize+' continuation='+this.state.continuation);
        let queryArgs = {
            table: selectedTable
        };

        // TODO: fix paging on lookup with duplicate-bag/bag entries, doing this hack to allow it to work
        // potentially crazy, if ETS backend has LOADS of entries, could crash things here.
        if(queryType != 'lookup'){
            queryArgs.pagesize = this.state.pagesize;
        }

        queryArgs.continuation = this.state.continuation;
        // when reached the end of match/match-object/select query pagination ( continuation==undefined )
        // make sure to set tupleWildcard
        if(!this.state.continuation && this.state.prevTupleWildcard){
            queryArgs.tuple_wildcard = this.state.prevTupleWildcard;
        }
        // when viewing table, then running match/match-object/select query.
        if(this.state.prevQuery = 'page' && tupleWildcard) {
            queryArgs.continuation = undefined;
        }

        let useTupleWildcard;
        // if calling func with tupleWildcard, then use it in query
        if(tupleWildcard){
            useTupleWildcard = tupleWildcard;
            queryArgs.tuple_wildcard = tupleWildcard;
        }
        if(!tupleWildcard && this.state.prevTupleWildcard){
            useTupleWildcard = this.state.prevTupleWildcard;
            queryArgs.tuple_wildcard = this.state.prevTupleWildcard;
        }

        if(queryType){
            queryArgs.query_type = queryType;
        }
        if(key){
            queryArgs.key = key;
        }

        console.log('query arguments ');
        Object.keys(queryArgs).forEach(prop => console.log('prop='+prop+' value='+queryArgs[prop]));
        const sr = this.setStateRows;
        $.get("/api/query", queryArgs)
        .done(function(data) {
            //console.log([selectedTable]);
            sr(selectedTable, data, queryType, useTupleWildcard)
        });
    }
    logState = () => {
        console.log('state.viewSystemTables = '+this.state.viewSystemTables);
        console.log('state.viewQuery = '+this.state.viewQuery);
        console.log('state.liveView = '+this.state.liveView);
        console.log('state.tables = '+this.state.tables);
        console.log('state.selectedTable = '+this.state.selectedTable);
        console.log('state.pagesize_options = '+this.state.pagesize_options);
        console.log('state.pagesize = '+this.state.pagesize);
        console.log('state.tableRows = '+this.state.tableRows);
        console.log('state.showTables = '+this.state.showTables);
        console.log('state.showTableRows = '+this.state.showTableRows);
        console.log('state.continuation = '+this.state.continuation);
        console.log('state.queryTables = '+this.state.queryTables);
        console.log('state.prevQuery = '+this.state.prevQuery);
    }
    nextEntries = () => {
        //console.log('fetch data for table '+this.state.selectedTable+' pagesize '+ this.state.pagesize);
        this.queryTable(this.state.selectedTable, this.state.prevQuery);
    }
    lookupQuery = () => {
        //console.log('lookupQuery');
        const key = document.getElementById('lookup_key').value;
        //console.log('key '+key);
        if(key){
            this.queryTable(this.state.selectedTable, 'lookup', key);
        } else {

        }
    }
    matchQuery = () => {
        const tupleWildcard = document.getElementById('match').value;
        this.queryTable(this.state.selectedTable, 'match', undefined, tupleWildcard);
    }
    matchObjectQuery = () => {
        const tupleWildcard = document.getElementById('match_object').value;
        this.queryTable(this.state.selectedTable, 'match_object', undefined, tupleWildcard);
    }
    changePagesize = (selectedObject) => {
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
        // NB! dumb assumption that hosting on HTTPS, will also have WSS calls to backend.
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
        // TODO: fetching tables at startup, below.
        this.fetchTables();
    }
    componentDidUpdate(prevProps, prevState){
        // extra action to fetching tables, when tabble view setting changes...
        // view system tables state changed (button press), fetch tables again...
        if( prevState.viewSystemTables != this.state.viewSystemTables ) {
            this.fetchTables();
        }
        // here we assume, that the user is viewing entries of the table,
        // below happens when chaning pagesize on entries view
        if( this.state.resetQuery ) {
            this.nextEntries();
        }
    }
    render(){
        //console.log('EtsUiApp.render()');
        return (
            <div>
                <EtsHeader
                    logState={this.logState}
                    toggleViewSystemTables={this.toggleViewSystemTables}
                    toggleViewQuery={this.toggleViewQuery}
                    viewSystemTables={this.state.viewSystemTables}
                    fetchTables={this.fetchTables}
                />
                <EtsQuery
                    viewQuery={this.state.viewQuery}
                    queryTables={this.state.queryTables}
                    lookupQuery={this.lookupQuery}
                    matchQuery={this.matchQuery}
                    matchObjectQuery={this.matchObjectQuery}
                />
                { this.state.showTables && this.tableView() }
                { this.state.showTableRows && this.entriesView() }
            </div>
        );
    }
}