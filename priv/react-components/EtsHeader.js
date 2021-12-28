import React from "react";
const EtsHeader = (props) => (
    <div>
        <h1>ETS-UI</h1>
        <button onClick={props.logState}>Log state</button>
        <button onClick={props.fetchTables} >Tables</button>
        <button onClick={props.toggleViewSystemTables}>{props.viewSystemTables ? "Hide" : "Show" } system tables</button>
        <button onClick={props.toggleViewQuery}>Query</button>
    </div>
);
export default EtsHeader;