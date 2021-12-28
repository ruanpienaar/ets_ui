import React from 'react';
const EtsQueryPanel = (props) => (
    <div>
        <table>
            <thead>
                <tr>
                    <th>selected tables</th>
                    <th>Query Type</th>
                    <th>Query Value</th>
                    <th>&nbsp;</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td rowSpan={4}>
                        <ul>
                            {props.queryTables.map((tbl) => (<li key={tbl}>{tbl}</li>))}
                        </ul>
                    </td>
                    <td>Lookup Key</td>
                    <td><input type="text" id="lookup_key" name="lookup_key" /></td>
                    <td><button disabled={props.queryTables.length <= 0 } onClick={props.lookupQuery}>Lookup</button></td>
                </tr>
                <tr>
                    <td>Match (*Tuple Expr)</td>
                    <td><input type="text" id="match" name="match" /></td>
                    <td><button disabled={props.queryTables.length <= 0 } onClick={props.matchQuery}>Match</button></td>
                </tr>
                <tr>
                    <td>Match Object (*Tuple Expr)</td>
                    <td><input type="text" id="match_object" name="match_object" /></td>
                    <td><button disabled={props.queryTables.length <= 0 } onClick={props.matchObjectQuery}>Match Object</button></td>
                </tr>
                <tr>
                    <td>MatchSpec (*Tuple Expr)</td>
                    <td><input type="text" id="select" name="select" /></td>
                    <td><button disabled={props.queryTables.length <= 0 } onClick={props.SelectQuery}>Select</button></td>
                </tr>
            </tbody>
        </table>
    </div>
);
export default EtsQueryPanel