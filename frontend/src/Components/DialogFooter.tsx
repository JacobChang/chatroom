import React from "react";
import "./DialogFooter.css";
import classNames from "classnames";
import { ClassValue } from "classnames/types";

export interface Props {
  className?: ClassValue[] | ClassValue;
}

export interface State {}

export class DialogFooter extends React.PureComponent<Props, State> {
  render() {
    const { className } = this.props;

    return (
      <div className={classNames("dialog__footer", className)}>
        {this.props.children}
      </div>
    );
  }
}
