import React from "react";
const EtsTableViewPagesize = (props) => (
    <select
        value={props.pagesize}
        onChange={props.changePagesize}
        name="pagesize"
        className="btn btn-primary"
    >
        {
            props.pagesize_options.map(
                (option) => (
                    <option key={option} value={option}>{option}</option>
                )
            )
        }
    </select>

);
export default EtsTableViewPagesize;