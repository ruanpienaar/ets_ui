import React from "react";
const EtsTableViewNextRows = (props) => (
    <button onClick={props.nextEntries}>{ props.continuation ? 'Next' : 'From Beginning' }</button>
);
export default EtsTableViewNextRows