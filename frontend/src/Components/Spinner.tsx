import React from "react";
import "./Spinner.css";
import classNames from "classnames";
import { ClassValue } from "classnames/types";

export interface Props {
  className?: ClassValue[] | ClassValue;
}

export class Spinner extends React.PureComponent<Props> {
  render() {
    return (
      <div className={classNames("spinner", this.props.className)}>
        <div className="cube" />
        <div className="cube" />
      </div>
    );
  }
}
