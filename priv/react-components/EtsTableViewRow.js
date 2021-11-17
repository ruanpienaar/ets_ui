import React from "react";

const EtsTableViewRow = (props) => {
    const erlang_tuple = '{' + props.values.map((c) => (c[1])).join(',') + '}'
    return (
        <div>
        {erlang_tuple}
        </div>
    );
}

export default EtsTableViewRow;
