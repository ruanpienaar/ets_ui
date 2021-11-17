import React from "react";

const Header = (props) => (
    <div>
        <h1>ETS-UI</h1>
        <h3>Settings</h3>
        <button onClick={props.logState}>Log state</button>
        <button onClick={props.toggleViewSystemTables}>
            {props.viewSystemTables ? "Hide" : "Show" } system tables
        </button>
        <button onClick={props.fetchTables} >Fetch tables</button>
        <hr />
    </div>
);

export default Header;