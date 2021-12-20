import React from "react";

const Header = (props) => (
    <div>
        <h1>ETS-UI</h1>
        <h3>Settings</h3>

        <div>
            <h3>Debug</h3>
            <button onClick={props.logState}>Log state</button>
        </div>

        <hr />

        <button onClick={props.fetchTables} >Tables</button>
        <button onClick={props.toggleViewSystemTables}>
            {props.viewSystemTables ? "Hide" : "Show" } system tables
        </button>
        <hr />
    </div>
);

export default Header;